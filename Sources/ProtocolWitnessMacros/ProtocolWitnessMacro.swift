import SwiftCompilerPlugin
import SwiftDiagnostics
import SwiftSyntax
import SwiftSyntaxBuilder
import SwiftSyntaxMacros

@main
struct ProtocolWitnessPlugin: CompilerPlugin {
    let providingMacros: [Macro.Type] = [
        ProtocolWitnessMacro.self
    ]
}

/// Implementation of the `@ProtocolWitness` attached macro. This macro adds a struct called `Witness`
/// to the class it's attached to. The struct contains member variables with closure types corresponding to all non-
/// generic functions in the class. The struct also contains a static `live` function for constructing the live instance
/// of the witness, which simply calls the underlying class from each closure implementation.
public struct ProtocolWitnessMacro: MemberMacro {
    /// Generates the `Witness` struct declaration with closure variables
    public static func expansion(
        of node: AttributeSyntax,
        providingMembersOf declaration: some DeclGroupSyntax,
        in context: some MacroExpansionContext
    ) throws -> [DeclSyntax] {
        let ignoreGenerics = shouldIgnoreGenerics(with: node)

        // Ensure that the declaration is a class
        guard let classDecl = declaration.as(ClassDeclSyntax.self) else {
            context.diagnose(Diagnostic(node: declaration, message: ProtocolWitnessDiagnostic.onlyClasses))
            return []
        }

        // Functions eligible to be witnessed
        let eligibleFunctions = classDecl.memberBlock.members.compactMap { blockItem -> FunctionDeclSyntax? in
            guard let function = blockItem.decl.as(FunctionDeclSyntax.self) else { return nil }
            guard function.genericParameterClause == nil else {
                if !ignoreGenerics {
                    context.diagnose(Diagnostic(node: function, message: ProtocolWitnessDiagnostic.noGenericProtocolWitnesses))
                }
                return nil
            }
            return function
        }

        // Generate the members of the Witness struct
        let witnessVariables = makeWitnessClosureVariables(functions: eligibleFunctions, in: context)
        let liveFunction = makeLiveFunction(classDecl: classDecl, functions: eligibleFunctions, witnessVariables: witnessVariables)
        let witnessMemberBlock = makeMemberBlock(with: witnessVariables + [liveFunction])

        let witnessDecl: DeclSyntax = """
        struct Witness {
            \(witnessMemberBlock)
        }
        """
        return [witnessDecl]
    }

    // MARK: - Private helpers

    /// Generate the member variables of the `Witness` struct
    private static func makeWitnessClosureVariables(functions: [FunctionDeclSyntax], in context: some MacroExpansionContext) -> [VariableDeclSyntax] {
        functions.map { function -> VariableDeclSyntax in
            var variableName = function.name.text
            var closureParameters: [TupleTypeElementSyntax] = []
            let functionParameters = function.signature.parameterClause.parameters
            for (index, parameter) in functionParameters.enumerated() {
                let trailing: (comma: TokenSyntax, trivia: Trivia)? = index == functionParameters.count - 1 ? nil : (.commaToken(), .space)
                if parameter.firstName.tokenKind == .wildcard {
                    let closureParam = TupleTypeElementSyntax(
                        firstName: .wildcardToken(trailingTrivia: .space),
                        secondName: parameter.secondName,
                        colon: .colonToken(),
                        type: parameter.type,
                        trailingComma: trailing?.comma,
                        trailingTrivia: trailing?.trivia
                    )
                    closureParameters.append(closureParam)
                } else {
                    variableName.append(parameter.firstName.text.capitalizeFirstLetter())
                    let closureParam = TupleTypeElementSyntax(
                        type: parameter.type,
                        trailingComma: trailing?.comma,
                        trailingTrivia: trailing?.trivia
                    )
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
    }

    /// Generate the "live" function of the `Witness` struct
    private static func makeLiveFunction(
        classDecl: ClassDeclSyntax,
        functions: [FunctionDeclSyntax],
        witnessVariables: [VariableDeclSyntax]
    ) -> DeclSyntax {
        let arguments: [LabeledExprSyntax] = zip(functions, witnessVariables).enumerated().compactMap { index, functionAndVariable -> LabeledExprSyntax? in
            let (function, variable) = functionAndVariable
            guard let label = variable.bindings.first?.pattern.as(IdentifierPatternSyntax.self)?.identifier else {
                assertionFailure("Failed to determine label")
                return nil
            }

            let functionParameters = function.signature.parameterClause.parameters
            let underlyingFunctionArguments: [LabeledExprSyntax] = functionParameters.enumerated().map { index, parameter in
                let label: TokenSyntax? = parameter.firstName.tokenKind == .wildcard ? nil : parameter.firstName.trimmed
                let expression: ExprSyntax = "$\(literal: index)"
                let trailing: (comma: TokenSyntax, trivia: Trivia)? = index == functionParameters.count - 1 ? nil : (.commaToken(), .space)
                return LabeledExprSyntax(
                    label: label,
                    colon: label != nil ? .colonToken() : nil,
                    expression: expression,
                    trailingComma: trailing?.comma,
                    trailingTrivia: trailing?.trivia
                )
            }
            let argumentList = LabeledExprListSyntax(underlyingFunctionArguments)
            var functionCallExpression: ExprSyntax = "underlying.\(function.name.trimmed)(\(argumentList))"
            if function.signature.effectSpecifiers?.asyncSpecifier != nil {
                functionCallExpression = "await \(functionCallExpression)"
            }
            if function.signature.effectSpecifiers?.throwsSpecifier != nil {
                functionCallExpression = "try \(functionCallExpression)"
            }

            let functionCallCodeBlock = CodeBlockItemSyntax(item: .expr(functionCallExpression))
            let closureExpression = ClosureExprSyntax(leftBrace: .leftBraceToken(), statements: [functionCallCodeBlock], rightBrace: .rightBraceToken())
            let addComma = index != witnessVariables.count - 1
            return LabeledExprSyntax(
                leadingTrivia: index == 0 ? nil : .newline,
                label: label,
                colon: .colonToken(),
                expression: closureExpression,
                trailingComma: addComma ? .commaToken() : nil
            )
        }
        let argumentList = LabeledExprListSyntax(arguments)

        return """

        static func live(_ underlying: \(classDecl.name.trimmed)) -> Witness {
            self.init(
                \(argumentList)
            )
        }
        """
    }

    private static func makeMemberBlock(with decls: [any DeclSyntaxProtocol]) -> MemberBlockItemListSyntax {
        let members = decls.enumerated().map { index, decl in
            MemberBlockItemSyntax(leadingTrivia: index == 0 ? nil : .newline, decl: decl)
        }
        return MemberBlockItemListSyntax(members)
    }

    private static func shouldIgnoreGenerics(with node: AttributeSyntax) -> Bool {
        guard
            let arguments = node.arguments?.as(LabeledExprListSyntax.self),
            let argument = arguments.first(where: { $0.label?.text == "ignoreGenericFunctions" }),
            let expression = argument.expression.as(BooleanLiteralExprSyntax.self)
        else {
            return false
        }
        return expression.literal.tokenKind == .keyword(.true)
    }
}

enum ProtocolWitnessDiagnostic: String, DiagnosticMessage {
    case unknownError
    case onlyClasses
    case noGenericProtocolWitnesses

    var message: String {
        switch self {
        case .unknownError:
            return "An unknown error occurred"
        case .onlyClasses:
            return "Protocol witnesses are only supported for classes at this time"
        case .noGenericProtocolWitnesses:
            return "Function with generic parameters will not be included in the protocol witness"
        }
    }

    var diagnosticID: MessageID {
        MessageID(domain: "ProtocolWitnessMacros", id: rawValue)
    }

    var severity: DiagnosticSeverity {
        switch self {
        case .unknownError, .onlyClasses:
            return .error
        case .noGenericProtocolWitnesses:
            return .warning
        }
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
