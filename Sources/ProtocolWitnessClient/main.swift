import ProtocolWitness

let a = 17
let b = 25

let (result, code) = #stringify(a + b)

print("The value \(result) was produced by the code \"\(code)\"")

final class MyAPI {
    struct Witness {
        var fetchUsers: () async throws -> [User]
        var saveUser: (_ user: User) async throws -> Void
    }

    struct User {}

    let apiToken: String

    init(apiToken token: String) {
        self.apiToken = token
    }

    func fetchUsers() async throws -> [User] {
        return []
    }

    func save(user: User) async throws {
    }

    func doSomethingGeneric<T>(value: T) {
    }
}

extension MyAPI.Witness {
    init(apiToken: String) {
        self.init(MyAPI(apiToken: apiToken))
    }

    init(_ underlying: MyAPI) {
        self.init(
            fetchUsers: {
                try await underlying.fetchUsers()
            },
            saveUser: {
                try await underlying.save(user: $0)
            }
        )
    }
}
