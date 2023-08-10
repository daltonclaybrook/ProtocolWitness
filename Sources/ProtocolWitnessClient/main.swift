import ProtocolWitness

let a = 17
let b = 25

let (result, code) = #stringify(a + b)

print("The value \(result) was produced by the code \"\(code)\"")

struct User {}

@ProtocolWitness
final class MyAPI {
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

func makeLiveAPI(apiToken: String) -> MyAPI.Witness {
    let underlying = MyAPI(apiToken: apiToken)
    return MyAPI.Witness(
        fetchUsers: {
            try await underlying.fetchUsers()
        },
        saveUser: {
            try await underlying.save(user: $0)
        }
    )
}

//extension MyAPI.Witness {
//    static func live(apiToken: String) -> Self {
//        let underlying = MyAPI(apiToken: apiToken)
//        return self.init(
//            fetchUsers: {
//                try await underlying.fetchUsers()
//            },
//            saveUser: {
//                try await underlying.save(user: $0)
//            }
//        )
//    }
//}
