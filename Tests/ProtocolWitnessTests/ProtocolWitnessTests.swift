import SwiftSyntaxMacros
import SwiftSyntaxMacrosTestSupport
import XCTest

// Macro implementations build for the host, so the corresponding module is not available when cross-compiling. Cross-compiled tests may still make use of the macro itself in end-to-end tests.
#if canImport(ProtocolWitnessMacros)
import ProtocolWitnessMacros

let testMacros: [String: Macro.Type] = [
    "stringify": StringifyMacro.self,
    "ProtocolWitness": ProtocolWitnessMacro.self
]


final class ProtocolWitnessTests: XCTestCase {
    func testMacro() throws {

        assertMacroExpansion(
            """
            #stringify(a + b)
            """,
            expandedSource: """
            (a + b, "a + b")
            """,
            macros: testMacros
        )
    }

    func testMacroWithStringLiteral() throws {
        assertMacroExpansion(
            #"""
            #stringify("Hello, \(name)")
            """#,
            expandedSource: #"""
            ("Hello, \(name)", #""Hello, \(name)""#)
            """#,
            macros: testMacros
        )
    }

    func testProtocolWitnessMacro() throws {
        assertMacroExpansion(
            #"""
            @ProtocolWitness
            final class MyAPI {
                struct User {}

                let apiToken: String

                init(apiToken token: String) {
                    self.apiToken = token
                }

                init(_ token: String) {
                    self.apiToken = token
                }

                init(fooBar: String) {
                    self.init(apiToken: fooBar)
                }

                func fetchUsers() async throws -> [User] {
                    return []
                }

                func save(user: User) async throws {
                }

                func doSomethingGeneric<T>(value: T) {
                }
            }
            """#,
            expandedSource: """
            """,
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
