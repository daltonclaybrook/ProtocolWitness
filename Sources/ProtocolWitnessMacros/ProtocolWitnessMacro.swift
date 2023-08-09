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
    public static func expansion(
        of node: AttributeSyntax,
        providingMembersOf declaration: some DeclGroupSyntax,
        in context: some MacroExpansionContext
    ) throws -> [DeclSyntax] {
        guard declaration.is(ClassDeclSyntax.self) else {
            throw ProtocolWitnessDiagnostic.onlyClasses
        }

        return []
    }

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

        var witnessInitializers = try initializers.map { initializer in
            try makeWitnessInitializer(parameters: initializer.signature.parameterClause.parameters)
        }
        if witnessInitializers.isEmpty {
            // If the class doesn't have any initializers, add an empty one to the witness
            try witnessInitializers.append(makeWitnessInitializer())
        }

        let blockItems = witnessInitializers.enumerated().map { index, initializer in
            MemberBlockItemSyntax(
                leadingTrivia: index == 0 ? nil : .newline,
                decl: initializer
            )
        }

        let witnessExtension: DeclSyntax = """
        extension \(classDecl.name).Witness {
            \(MemberBlockItemListSyntax(blockItems))
        }
        """
        return [witnessExtension]
    }

    // MARK: - Private helpers

    private static func makeWitnessInitializer(parameters: FunctionParameterListSyntax = []) throws -> InitializerDeclSyntax {
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
            self.init(MyAPI(\(LabeledExprListSyntax(arguments))))
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

enum ProtocolWitnessDiagnostic: String, DiagnosticMessage, Error {
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
