# Protocol Witness Macro

A [Swift Macro](https://docs.swift.org/swift-book/documentation/the-swift-programming-language/macros/) for generating
[protocol witnesses](https://www.pointfree.co/collections/protocol-witnesses/alternatives-to-protocols) for your classes
and actors.

## Overview

When writing classes in Swift that are dependencies of other types, it's often helpful to express those dependencies
using an abstraction to aid in testability. The language feature we have traditionally reached for to solve this problem
is a [protocol](https://docs.swift.org/swift-book/documentation/the-swift-programming-language/protocols/). Given a
class called `APIClient`, we might choose to create a protocol called `APIClientType` along with a conformance by the
class. This enables us to create a separate type called `MockAPIClient` for use in our tests.

But this approach isn't perfect. Dependening on the size of our `APIClient`, this can involve a lot of boilerplate code.
And `MockAPIClient` can only conform to `APIClientType` once, so we have to make that conformance flexible enough to
support all the different kinds of tests we would want to write. Soon enough, our mock will be so complex that we'll be
wondering if we should be writing tests for it instead.

## Protocol Witnesses

One solution to this problem of rigitity, complexity, and boilerplate is to use a protocol witness instead. Simply put,
a protocol witness is a struct representation of a protocol. The term comes from the Swift compiler itself—when you
compile Swift code with a protocol, the compiler generates a protocol witness under the hood. Instead of function
requirements, the struct contains closure variables.

```swift
/// Protocol
protocol APIClientType {
    func findAuthors(named: String) async throws -> [Author]
    func fetchBlogPosts(by author: Author) async throws -> [Post]
}

/// Protocol witness
struct APIClient {
    var findAuthorsNamed: (String) async throws -> [Author]
    var fetchBlogPostsByAuthor: (Author) async throws -> [Post]
}
```

Using a protocol witness instead of a protocol means that we can create as many variations of the `APIClient` as we want
without needing a bunch of new types and boilerplate. In our tests, we can replace function implementation on a
test-by-test basis.

But this strategy is not without its faults either. What if your witness needs to manage a lot of mutable state? You
could capture state variables in your witness closures, but eventually this starts to feel like we're fighting the
system, especially when we could be using a class (or actor). What if we could build our dependency as a class like we'd
prefer while also leveraging the flexibiliy of protocol witnesses for abstraction and testability?

## The Macro

This project aims to solve the problem decribed above. You can build your dependency as a class or actor, then annotate
it with the `@ProtocolWitness` macro. At compile time, the macro generates a new type for you under the namespace of
your dependency called `Witness`. This new type is a struct that mirrors your class/actor using closures instead of
functions and properties. The `Witness` type contains a static function called `live` for instantiating the "live"
version of your dependency using an instance of your class/actor to implement the closures. For example:

```swift
// Given the following class, annotated with the macro:

@ProtocolWitness
final class APIClient {
    private let session = URLSession.shared

    func fetchAuthors(named: String) async throws -> [Author] {
        let (data, _) = try await session.data(from: .authorsURL(name: named))
        return try JSONDecoder().decode([Author].self, from: data)
    }
}

// The compiler will generate a type called "Witness" under the `APIClient` namespace:

final class APIClient {
    ...

    struct Witness {
        var fetchAuthorsNamed: (String) async throws -> [Author]

        static func live(_ underlying: APIClient) -> Witness {
            self.init(
                fetchAuthorsNamed: {
                    try await underlying.fetchAuthors(named: $0)
                }
            )
        }
    }
}
```

The following is an example of how to use the generated type in production code:

```swift
final class MyViewModel: ObservableObject {
    @Published var authors: [Author] = []

    private let apiClient: APIClient.Witness

    init(apiClient: APIClient.Witness = .live(APIClient())) {
        self.apiClient = apiClient
    }

    func onAppear() async throws {
        self.authors = try await apiClient.fetchAuthorsNamed("Jimmy")
    }
}
```

And in your tests:

```swift
func test_whenViewAppears_authorsAreFetched() async throws {
    var fetchedAuthor: String?
    let viewModel = MyViewModel(apiClient: .init(
        fetchAuthorsNamed: { fetchedAuthor = $0; return [] }
    ))
    try await viewModel.onAppear()
    XCTAssertEqual(fetchedAuthor, "Jimmy")
}
```
