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