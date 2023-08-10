import SwiftSyntaxMacros
import SwiftSyntaxMacrosTestSupport
import XCTest

// Macro implementations build for the host, so the corresponding module is not available when cross-compiling. Cross-compiled tests may still make use of the macro itself in end-to-end tests.
#if canImport(ProtocolWitnessMacros)
import ProtocolWitnessMacros

let testMacros: [String: Macro.Type] = [
    "ProtocolWitness": ProtocolWitnessMacro.self
]


final class ProtocolWitnessTests: XCTestCase {
    func testBasicProtocolWitnessIsGenerated() {
        assertMacroExpansion(
            """
            @ProtocolWitness
            class FooBar {
                func doSomething() {}
                func doSomethingGeneric<T>(foo: T) {}
                func fetch(userID: String) async throws -> User? { nil }
                func fetchUser(byName name: String) async throws -> User? { nil }
                func fetchUserName(_ name: String) async throws -> User? { nil }
                func searchUsers(name: String, age: Int) async throws -> [User] { [] }
            }
            """,
            expandedSource: """
            class FooBar {
                func doSomething() {}
                func doSomethingGeneric<T>(foo: T) {}
                func fetch(userID: String) async throws -> User? { nil }
                func fetchUser(byName name: String) async throws -> User? { nil }
                func fetchUserName(_ name: String) async throws -> User? { nil }
                func searchUsers(name: String, age: Int) async throws -> [User] { [] }

                struct Witness {
                    var doSomething: () -> Void
                    var fetchUserID: (String) async throws -> User?
                    var fetchUserByName: (String) async throws -> User?
                    var fetchUserName: (_ name: String) async throws -> User?
                    var searchUsersNameAge: (String, Int) async throws -> [User]

                    static func live(_ underlying: FooBar) -> Witness {
                        self.init(
                             doSomething: {
                                 underlying.doSomething()
                             },
                             fetchUserID: {
                                 try await underlying.fetch(userID: $0)
                             },
                             fetchUserByName: {
                                 try await underlying.fetchUser(byName: $0)
                             },
                             fetchUserName: {
                                 try await underlying.fetchUserName($0)
                             },
                             searchUsersNameAge: {
                                 try await underlying.searchUsers(name: $0, age: $1)
                             }
                        )
                    }
                }
            }
            """,
            diagnostics: [
                DiagnosticSpec(message: "Function with generic parameters will not be included in the protocol witness", line: 4, column: 5, severity: .note)
            ],
            macros: testMacros
        )
    }

    func test_ifAttachedToNonClass_diagnosticIsThrown() throws {
        assertMacroExpansion(
            #"""
            @ProtocolWitness
            struct MyStruct {
                init() {}
            }
            @ProtocolWitness
            enum MyEnum {
                case foo
            }
            """#,
            expandedSource: """
            struct MyStruct {
                init() {}
            }
            enum MyEnum {
                case foo
            }
            """,
            diagnostics: [
                .init(message: "Protocol witnesses are only supported for classes at this time", line: 1, column: 1),
                .init(message: "Protocol witnesses are only supported for classes at this time", line: 5, column: 1)
            ],
            macros: testMacros
        )
    }
}
#endif
