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

struct DeclPair {
    enum Source {
        case function(FunctionDeclSyntax)
        case variable(VariableDeclSyntax)
    }

    let sourceDecl: Source
    let witnessDecl: VariableDeclSyntax
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

        // Generate witness closure variables for properties and functions
        let witnessDeclPairs = classDecl.memberBlock.members.compactMap { blockItem -> DeclPair? in
            if let function = blockItem.decl.as(FunctionDeclSyntax.self) {
                return makeClosure(for: function, in: context, ignoreGenerics: ignoreGenerics)
            } else if let variable = blockItem.decl.as(VariableDeclSyntax.self) {
                return makeClosure(for: variable, in: context)
            } else {
                return nil
            }
        }

        // Generate the witness member block
        let liveFunction = makeLiveFunction(classDecl: classDecl, pairs: witnessDeclPairs)
        let witnessVariables = witnessDeclPairs.map(\.witnessDecl)
        let witnessMemberBlock = makeMemberBlock(with: witnessVariables + [liveFunction])

        let witnessDecl: DeclSyntax = """
        struct Witness {
            \(witnessMemberBlock)
        }
        """
        return [witnessDecl]
    }

    // MARK: - Private helpers

    private static func makeClosure(for function: FunctionDeclSyntax, in context: some MacroExpansionContext, ignoreGenerics: Bool) -> DeclPair? {
        guard function.genericParameterClause == nil else {
            if !ignoreGenerics {
                context.diagnose(Diagnostic(node: function, message: ProtocolWitnessDiagnostic.noGenericProtocolWitnesses))
            }
            return nil
        }

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
        let witnessVariable = makeWitnessClosureVariable(
            name: variableName,
            parameters: closureParameters,
            effects: functionEffects,
            returnType: function.signature.returnClause?.type
        )
        return DeclPair(sourceDecl: .function(function), witnessDecl: witnessVariable)
    }

    private static func makeClosure(for variable: VariableDeclSyntax, in context: some MacroExpansionContext) -> DeclPair? {
        guard variable.bindings.count == 1, let binding = variable.bindings.first else {
            context.diagnose(Diagnostic(node: variable, message: ProtocolWitnessDiagnostic.onlyOneBinding))
            return nil
        }
        guard let typeAnnotation = binding.typeAnnotation else {
            context.diagnose(Diagnostic(node: variable, message: ProtocolWitnessDiagnostic.missingTypeForVariable))
            return nil
        }
        guard let pattern = binding.pattern.as(IdentifierPatternSyntax.self) else {
            context.diagnose(Diagnostic(node: binding, message: ProtocolWitnessDiagnostic.unexpected(message: "Expected an identifier")))
            return nil
        }

        let witnessVariable = makeWitnessClosureVariable(name: pattern.identifier.text, returnType: typeAnnotation.type)
        return DeclPair(sourceDecl: .variable(variable), witnessDecl: witnessVariable)
    }

    private static func makeWitnessClosureVariable(
        name: String,
        parameters: [TupleTypeElementSyntax] = [],
        effects: FunctionEffectSpecifiersSyntax? = nil,
        returnType: TypeSyntax?
    ) -> VariableDeclSyntax {
        let voidReturnType = IdentifierTypeSyntax(name: .identifier("Void"))
        return VariableDeclSyntax(bindingSpecifier: .keyword(.var)) {
            PatternBindingSyntax(
                leadingTrivia: .space,
                pattern: IdentifierPatternSyntax(identifier: .identifier(name)),
                typeAnnotation: TypeAnnotationSyntax(
                    colon: .colonToken(),
                    type: FunctionTypeSyntax(
                        parameters: TupleTypeElementListSyntax(parameters),
                        effectSpecifiers: TypeEffectSpecifiersSyntax(
                            asyncSpecifier: effects?.asyncSpecifier,
                            throwsSpecifier: effects?.throwsSpecifier
                        ),
                        returnClause: ReturnClauseSyntax(
                            arrow: .arrowToken(),
                            type: returnType ?? voidReturnType.as(TypeSyntax.self)!
                        )
                    )
                )
            )
        }
    }

    /// Generate the "live" function of the `Witness` struct
    private static func makeLiveFunction(classDecl: ClassDeclSyntax, pairs: [DeclPair]) -> DeclSyntax {
        let arguments: [LabeledExprSyntax] = pairs.enumerated().compactMap { index, pair -> LabeledExprSyntax? in
            guard case .function(let function) = pair.sourceDecl else {
                // TODO: Implement for variables
                return nil
            }

            let variable = pair.witnessDecl
            guard let label = variable.bindings.first?.pattern.as(IdentifierPatternSyntax.self)?.identifier else {
                assertionFailure("Failed to determine label")
                return nil
            }

            let functionCallExpression = makeLiveFunctionCallExpression(for: function)
            let functionCallCodeBlock = CodeBlockItemSyntax(item: .expr(functionCallExpression))
            let closureExpression = ClosureExprSyntax(leftBrace: .leftBraceToken(), statements: [functionCallCodeBlock], rightBrace: .rightBraceToken())
            let addComma = index != pairs.count - 1
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

    private static func makeLiveFunctionCallExpression(for function: FunctionDeclSyntax) -> ExprSyntax {
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
        return functionCallExpression
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

enum ProtocolWitnessDiagnostic: DiagnosticMessage {
    case onlyClasses
    case noGenericProtocolWitnesses
    case onlyOneBinding
    case missingTypeForVariable
    case unexpected(message: String)

    var message: String {
        switch self {
        case .onlyClasses:
            return "Protocol witnesses are only supported for classes at this time"
        case .noGenericProtocolWitnesses:
            return "Function with generic parameters will not be included in the protocol witness"
        case .onlyOneBinding:
            return "Variable declarations with multiple bindings will not be included in the protocol witness"
        case .missingTypeForVariable:
            return "Variables without a type annotation will not be included in the protocol witness"
        case .unexpected(let message):
            return "Unexpected condition in protocol witness: \(message). Please file a bug report."
        }
    }

    var diagnosticID: MessageID {
        MessageID(domain: "ProtocolWitnessMacros", id: String(describing: self))
    }

    var severity: DiagnosticSeverity {
        switch self {
        case .onlyClasses, .unexpected:
            return .error
        case .noGenericProtocolWitnesses, .onlyOneBinding, .missingTypeForVariable:
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
