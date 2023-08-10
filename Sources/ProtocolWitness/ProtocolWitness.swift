/// This macro adds a struct called `Witness` to the class it's attached to. The struct contains
/// member variables with closure types corresponding to all non-generic functions in the class. 
/// The struct also contains a static `live` function for constructing the live instance of the
/// witness, which simply calls the underlying class from each closure implementation.
@attached(member, names: named(Witness))
public macro ProtocolWitness() = #externalMacro(module: "ProtocolWitnessMacros", type: "ProtocolWitnessMacro")
