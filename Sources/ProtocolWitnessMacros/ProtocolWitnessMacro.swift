import SwiftCompilerPlugin
import SwiftDiagnostics
import SwiftSyntax
import SwiftSyntaxBuilder
import SwiftSyntaxMacros

/// Implementation of the `stringify` macro, which takes an expression
/// of any type and produces a tuple containing the value of that expression
/// and the source code that produced the value. For example
///
///     #stringify(x + y)
///
///  will expand to
///
///     (x + y, "x + y")
public struct StringifyMacro: ExpressionMacro {
    public static func expansion(
        of node: some FreestandingMacroExpansionSyntax,
        in context: some MacroExpansionContext
    ) -> ExprSyntax {
        guard let argument = node.argumentList.first?.expression else {
            fatalError("compiler bug: the macro does not have any arguments")
        }

        return "(\(argument), \(literal: argument.description))"
    }
}

public struct ProtocolWitnessMacro: MemberMacro, PeerMacro {
    /// Generates the `Witness` struct declaration with closure variables
    public static func expansion(
        of node: AttributeSyntax,
        providingMembersOf declaration: some DeclGroupSyntax,
        in context: some MacroExpansionContext
    ) throws -> [DeclSyntax] {
        guard let classDecl = declaration.as(ClassDeclSyntax.self) else {
            context.diagnose(Diagnostic(node: declaration, message: ProtocolWitnessDiagnostic.onlyClasses))
            return []
        }

        let witnessVariables: [VariableDeclSyntax] = classDecl.memberBlock.members.compactMap { blockItem -> VariableDeclSyntax? in
            guard let function = blockItem.decl.as(FunctionDeclSyntax.self) else { return nil }

            var variableName = function.name.text
            var closureParameters: [TupleTypeElementSyntax] = []
            for parameter in function.signature.parameterClause.parameters {
                if parameter.firstName.tokenKind == .wildcard {
                    let closureParam = TupleTypeElementSyntax(firstName: .wildcardToken(), secondName: parameter.secondName, colon: .colonToken(), type: parameter.type)
                    closureParameters.append(closureParam)
                } else {
                    variableName.append(parameter.firstName.text.capitalizeFirstLetter())
                    let closureParam = TupleTypeElementSyntax(type: parameter.type)
                    closureParameters.append(closureParam)
                }
            }

            let functionEffects = function.signature.effectSpecifiers
            let voidReturnType = IdentifierTypeSyntax(name: .identifier("Void"))
            return VariableDeclSyntax(bindingSpecifier: .keyword(.var)) {
                PatternBindingSyntax(
                    leadingTrivia: .space,
                    pattern: IdentifierPatternSyntax(identifier: .identifier(variableName)),
                    typeAnnotation: TypeAnnotationSyntax(
                        colon: .colonToken(),
                        type: FunctionTypeSyntax(
                            parameters: TupleTypeElementListSyntax(closureParameters),
                            effectSpecifiers: TypeEffectSpecifiersSyntax(
                                asyncSpecifier: functionEffects?.asyncSpecifier,
                                throwsSpecifier: functionEffects?.throwsSpecifier
                            ),
                            returnClause: ReturnClauseSyntax.init(
                                arrow: .arrowToken(),
                                type: (function.signature.returnClause?.type) ?? voidReturnType.as(TypeSyntax.self)!
                            )
                        )
                    )
                )
            }
        }

        let witnessMemberBlockItems = witnessVariables.enumerated().map { index, variable in
            MemberBlockItemSyntax(leadingTrivia: index == 0 ? nil : .newline, decl: variable)
        }

        let structDecl: DeclSyntax = """
        struct Witness {
            \(MemberBlockItemListSyntax(witnessMemberBlockItems))
        }
        """
        return [structDecl]
    }
    
    /// Generates an extension for the `Witness` that adds the live implementation
    public static func expansion(
        of node: AttributeSyntax,
        providingPeersOf declaration: some DeclSyntaxProtocol,
        in context: some MacroExpansionContext
    ) throws -> [DeclSyntax] {
        guard let classDecl = declaration.as(ClassDeclSyntax.self) else {
            return []
        }

        let memberBlock = classDecl.memberBlock
        let initializers = memberBlock.members.compactMap { $0.decl.as(InitializerDeclSyntax.self) }

        var witnessInitializers = initializers.map { initializer in
            makeWitnessInitializer(className: classDecl.name, parameters: initializer.signature.parameterClause.parameters)
        }
        if witnessInitializers.isEmpty {
            // If the class doesn't have any initializers, add an empty one to the witness
            witnessInitializers.append(makeWitnessInitializer(className: classDecl.name))
        }

        let blockItems = witnessInitializers.enumerated().map { index, initializer in
            MemberBlockItemSyntax(
                leadingTrivia: index == 0 ? nil : .newline,
                decl: initializer
            )
        }

        let witnessExtension: DeclSyntax = """
        extension \(classDecl.name.trimmed).Witness {
            \(MemberBlockItemListSyntax(blockItems))
        }
        """
        return [witnessExtension]
    }

    // MARK: - Private helpers

    private static func makeWitnessInitializer(className: TokenSyntax, parameters: FunctionParameterListSyntax = []) -> InitializerDeclSyntax {
        let arguments: [LabeledExprSyntax] = parameters.compactMap { parameter in
            if parameter.firstName.tokenKind == .wildcard {
                guard let secondName = parameter.secondName else {
                    return nil
                }
                return LabeledExprSyntax(expression: DeclReferenceExprSyntax(baseName: secondName))
            } else if let secondName = parameter.secondName {
                return LabeledExprSyntax(label: parameter.firstName, colon: .colonToken(), expression: DeclReferenceExprSyntax(baseName: secondName))
            } else {
                return LabeledExprSyntax(label: parameter.firstName, colon: .colonToken(), expression: DeclReferenceExprSyntax(baseName: parameter.firstName))
            }
        }

        let syntax: DeclSyntax = """
        init(\(parameters)) {
            self.init(\(className.trimmed)(\(LabeledExprListSyntax(arguments))))
        }
        """
        return syntax.as(InitializerDeclSyntax.self)!
    }
}

@main
struct ProtocolWitnessPlugin: CompilerPlugin {
    let providingMacros: [Macro.Type] = [
        StringifyMacro.self,
        ProtocolWitnessMacro.self
    ]
}

enum ProtocolWitnessDiagnostic: String, DiagnosticMessage {
    case unknownError
    case onlyClasses

    var message: String {
        switch self {
        case .unknownError:
            return "An unknown error occurred"
        case .onlyClasses:
            return "Protocol witnesses are only supported for classes at this time"
        }
    }

    var diagnosticID: MessageID {
        MessageID(domain: "ProtocolWitnessMacros", id: rawValue)
    }

    var severity: DiagnosticSeverity {
        .error
    }
}

private extension String {
    func capitalizeFirstLetter() -> String {
        guard count > 0 else { return self }
        let firstLetter = prefix(1).capitalized
        if count == 1 {
            return firstLetter
        } else {
            let restOfString = self[index(after: startIndex)...]
            return "\(firstLetter)\(restOfString)"
        }
    }
}
