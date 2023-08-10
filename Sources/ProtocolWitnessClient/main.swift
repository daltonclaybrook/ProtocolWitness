import ProtocolWitness

struct User {}

@ProtocolWitness
class MyAPI {
    let apiToken: String

    init(apiToken: String) {
        self.apiToken = apiToken
    }

    func fetch(userID: String) async throws -> User { User() }
    func findUser(byName name: String) async throws -> User? { nil }
    func saveUser(_ user: User) async throws { }
    func searchUsers(name: String, age: Int) async throws -> [User] { [] }
    func doSomethingGeneric<T>(foo: T) {}
}

func useTheAPI() async throws {
    let api: MyAPI.Witness = .live(MyAPI(apiToken: "abc123"))
    let user = try await api.findUserByName("Dalton")
    print("Found user: \(String(describing: user))")
}

extension MyAPI {
    static func mockWitness() -> MyAPI.Witness {
        .init(
            fetchUserID: { _ in User() },
            findUserByName: { _ in nil },
            saveUser: { _ in },
            searchUsersNameAge: { _, _ in [] }
        )
    }
}
