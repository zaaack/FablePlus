[<AutoOpen>]
module FablePlus.Utils

open Fable.Core


///**Description**
/// Util function to set deep records/class like linq in dotnet or https://github.com/kube/monolite in typescript
/// e.g.
///  type Address = { name: string }
///  type People = {
///    name: string
///    age: int
///    address: Address
///  }
///  let tom = { name = "Tom"; age = 23; address = { name = "London" } }
///  let tom2 = setIn (fun t -> t.address.name) "Paris" tom
///  // tom2.address.name: Paris
///**Parameters**
///  * `'a -> 'b` - keyPath, a lambda expression to get the key path
///  * `'b` - value, the new value to set
///  * `'a` - state, the state record/object to update value
///**Output Type**
///  * `'a` - newState, the new state after update key
///
///**Exceptions**
/// Invalid setter: if the keyPath is not a valid lambda expression
[<Import("setIn", "./Utils.js")>]
let setIn: keyPath:('a -> 'b) -> value:'b -> state:'a -> 'a = fun _ _ a  -> a

