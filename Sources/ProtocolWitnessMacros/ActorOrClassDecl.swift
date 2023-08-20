import Foundation
import SwiftSyntax

protocol ActorOrClassDecl: DeclSyntaxProtocol {
    var name: TokenSyntax { get }
}

extension ActorDeclSyntax: ActorOrClassDecl {}
extension ClassDeclSyntax: ActorOrClassDecl {}

extension ActorOrClassDecl {
    var isActor: Bool {
        self.is(ActorDeclSyntax.self)
    }

    var isClass: Bool {
        self.is(ClassDeclSyntax.self)
    }
}
