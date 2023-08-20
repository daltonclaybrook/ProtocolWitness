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
                func fetch(userID: String) async throws -> User? { nil }
                func fetchUser(byName name: String) async throws -> User? { nil }
                func fetchUserName(_ name: String) async throws -> User? { nil }
                func searchUsers(name: String, age: Int) async throws -> [User] { [] }
            }
            """,
            expandedSource: """
            class FooBar {
                func doSomething() {}
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
            macros: testMacros
        )
    }

    func testClosuresAreGeneratedForProperties() {
        assertMacroExpansion(
            """
            @ProtocolWitness
            class MyAPI {
                var foo: Int = 123
                let bar = "abc"
                var fizz: Int { 123 }
                var buzz: Int

                init(buzz: Int) {
                    self.buzz = buzz
                }
            }
            """,
            expandedSource: """
            class MyAPI {
                var foo: Int = 123
                let bar = "abc"
                var fizz: Int { 123 }
                var buzz: Int

                init(buzz: Int) {
                    self.buzz = buzz
                }

                struct Witness {
                    var foo: () -> Int
                    var fizz: () -> Int
                    var buzz: () -> Int

                    static func live(_ underlying: MyAPI) -> Witness {
                        self.init(
                            foo: {
                                underlying.foo
                            },
                            fizz: {
                                underlying.fizz
                            },
                            buzz: {
                                underlying.buzz
                            }
                        )
                    }
                }
            }
            """,
            macros: testMacros
        )
    }

    func testWitnessIsGeneratedForActor() {
        assertMacroExpansion(
            """
            @ProtocolWitness
            actor MyAPI {
                func doSomething()
            }
            """,
            expandedSource: """
            class MyAPI {
                func doSomething()

                struct Witness {
                    var doSomething: () async -> Void

                    static func live(_ underlying: MyAPI) -> Witness {
                        self.init(
                            doSomething: {
                                await underlying.doSomething()
                            }
                        )
                    }
                }
            }
            """,
            macros: testMacros
        )
    }

    func test_ifGenericsAreNotIgnored_diagnosticsAreEmitted() {
        assertMacroExpansion(
            """
            @ProtocolWitness
            class MyAPI {
                func doSomethingGeneric<T>(foo: T) {}
            }
            """,
            expandedSource: """
            class MyAPI {
                func doSomethingGeneric<T>(foo: T) {}

                struct Witness {

                    static func live(_ underlying: MyAPI) -> Witness {
                        self.init(

                        )
                    }
                }
            }
            """,
            diagnostics: [
                DiagnosticSpec(message: "Function with generic parameters will not be included in the protocol witness", line: 3, column: 5, severity: .warning)
            ],
            macros: testMacros
        )
    }

    func test_ifGenericsAreIgnored_noDiagnosticsAreEmitted() {
        assertMacroExpansion(
            """
            @ProtocolWitness(ignoreGenericFunctions: true)
            class MyAPI {
                func doSomethingGeneric<T>(foo: T) {}
            }
            """,
            expandedSource: """
            class MyAPI {
                func doSomethingGeneric<T>(foo: T) {}

                struct Witness {

                    static func live(_ underlying: MyAPI) -> Witness {
                        self.init(

                        )
                    }
                }
            }
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
