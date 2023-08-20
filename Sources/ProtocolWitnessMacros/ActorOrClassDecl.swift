import Foundation
import SwiftSyntax

protocol ActorOrClassDecl {
    var name: TokenSyntax { get }
}

extension ActorDeclSyntax: ActorOrClassDecl {}
extension ClassDeclSyntax: ActorOrClassDecl {}
