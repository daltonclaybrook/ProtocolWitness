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
