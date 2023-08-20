import Combine
import Foundation
import ProtocolWitness

struct Post {}

@ProtocolWitness(ignoreGenericFunctions: true)
class BlogAPI {
    let apiToken: String

    init(apiToken: String) {
        self.apiToken = apiToken
    }

    func fetchPost(byID id: String) async throws -> Post? { nil }
    func fetchAllPosts() async throws -> [Post] { [] }
    func createPost(_ post: Post) async throws { }
    func searchPosts(byAuthor: String, beforeDate: Date) async throws -> [Post] { [] }
    func doSomethingGeneric<T>(foo: T) { }
}

@ProtocolWitness
actor MyThreadSafeType {
    var counter: Int = 0

    func increment() {
        counter += 1
    }

    func decrement() {
        counter -= 1
    }
}

@ProtocolWitness
final class MyGenericType<Foo: Equatable, Bar> {
    let foo: Foo

    init(foo: Foo) {
        self.foo = foo
    }

    func doStuff(with bar: Bar) {}
}

@MainActor
class BlogPostsViewModel: ObservableObject {
    @Published var posts: [Post] = []

    private let api: BlogAPI.Witness
    init(api: BlogAPI.Witness = .live(BlogAPI(apiToken: "abc123"))) {
        self.api = api
    }

    func onAppear() {
        Task {
            self.posts = try await api.fetchAllPosts()
        }
    }
}
