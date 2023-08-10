import Combine
import Foundation
import ProtocolWitness

struct Post {}

@ProtocolWitness
class MyAPI {
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
class MyViewModel: ObservableObject {
    @Published var posts: [Post] = []

    private let api: MyAPI
    init(api: MyAPI) {
        self.api = api
    }

    func onAppear() {
        Task {
            self.posts = try await api.fetchAllPosts()
        }
    }
}
