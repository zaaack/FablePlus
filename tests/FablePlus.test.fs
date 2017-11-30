module FablePlus.Tests.Tests

open System
open Fable.Core
open Fable.Core.JsInterop
open Fable.Core.Testing
open Fable.Import
open Fable.Import.Node
open Fable.Import
open Fable.PowerPack
open FablePlus.Tests.Utils
open FablePlus
open System.Collections.Generic
open Fable.Import.JS

let equal(left, right, str) =
  if left <> right then
    printfn "left: %A right: %A" left right
  Assert.AreEqual(left, right, str)

let f a = a + 1
describe "test map" <| fun () ->
  it "should option work" <| fun () ->
    let a = Some 1
    equal(a |>> f, Some 2, "option")

  it "should Result work" <| fun _ ->
    let a: Result<int, string> = Ok 1
    equal(a |>> f, Ok 2, "Result")

  it "should Lazy work" <| fun _ ->
    let a = Lazy<_>.CreateFromValue 1
    let a = a |>> f
    equal(a.Force(), 2, "Lazy")

  it "should Choice work" <| fun _ ->
    let a = Choice1Of2 1
    let a = a |>> f
    equal(a, Choice1Of2 2, "left")

    let a: Choice<int, int> = Choice2Of2 1
    let a = a |>> f
    equal(a, Choice2Of2 1, "right")

  it "should function work" <| fun _ ->
    let a = fun _ -> 1
    let a = a |>> f
    equal(a (),  2, "left")

  itAsync "should async work" <| fun _ ->
    async {
      let aAsync = async.Return 1
      let aAsync2 = aAsync |>> f
      let! a = aAsync2
      equal(a, 2, "async")
      return ()
    } |> Async.StartAsPromise

  itAsync "should promise work" <| fun _ ->
    async {
      let aPromise = promise.Return 1
      let aPromise2 = aPromise |>> f
      let! a = aPromise2 |> Async.AwaitPromise
      equal(a, 2, "async")
      return ()
    } |> Async.StartAsPromise


describe "test bind" <| fun () ->
  it "should option work" <| fun () ->
    let a = Some 1
    equal(a >>= (f >> Some), Some 2, "option")

  it "should Result work" <| fun _ ->
    let a: Result<int, string> = Ok 1
    equal(a >>= (f >> Result.Ok), Ok 2, "Result")

  it "should Lazy work" <| fun _ ->
    let a = Lazy<_>.CreateFromValue 1
    let a = a >>= (f >> Lazy<_>.CreateFromValue)
    equal(a.Force(), 2, "Lazy")

  it "should Choice work" <| fun _ ->
    let a = Choice1Of2 1
    let a = a |>> f
    equal(a, Choice1Of2 2, "left")

    let a: Choice<int, int> = Choice2Of2 1
    let a = a |>> f
    equal(a, Choice2Of2 1, "right")

  it "should function work" <| fun _ ->
    let a = fun _ -> 1
    let a = a |>> f
    equal(a (),  2, "left")

  itAsync "should async work" <| fun _ ->
    async {
      let aAsync = async.Return 1
      let aAsync2 = aAsync |>> f
      let! a = aAsync2
      equal(a, 2, "async")
      return ()
    } |> Async.StartAsPromise

  itAsync "should promise work" <| fun _ ->
    async {
      let aPromise = promise.Return 1
      let aPromise2 = aPromise |>> f
      let! a = aPromise2 |> Async.AwaitPromise
      equal(a, 2, "async")
      return ()
    } |> Async.StartAsPromise


type Address = { name: string }
type Hobby = { name: string }
type People = {
  name: string
  age: int
  address: Address
  hobbies: Hobby list
}
with
  member x.A() =
    x.name


printfn "aaa"

describe "test utils" <| fun _ ->
  it "setIn should work" <| fun _ ->
    let tom = { name = "Tom"; age = 23;
                address = { name = "London" };
                hobbies=
                  [ { name="music" }
                    { name="football" } ]
              }
    let tom2 = setIn (fun t -> t.address.name) "Paris" tom

    equal (tom.address.name, "London", "old should keep the smae")

    equal (tom2.address.name, "Paris", "new should change")

    equal (obj.ReferenceEquals (tom, tom2), false, "ref equal should be false")

    equal (obj.ReferenceEquals (tom.name, tom2.name), true, "name should be the same")

    equal (obj.ReferenceEquals (tom.hobbies, tom2.hobbies), true, "hobbies(refs) should be the same")

    equal (tom.hobbies, tom2.hobbies, "hobbies(value) should be the same")


    let tom2 = setIn (fun t -> t.hobbies.[0].name) "reading" tom

    equal (tom.address.name, "London", "old should keep the smae 2")

    equal (tom2.address, tom.address, "address should keep the smae")

    equal (obj.ReferenceEquals (tom.address, tom2.address), true, "address (ref) should keep the smae")

    equal (obj.ReferenceEquals (tom, tom2), false, "tom ref should change after change list")

    equal (
      tom.hobbies,
      [ { name="music" }
        { name="football" } ],
      "new hobbies should equal")

    equal (
      tom2.hobbies,
      [ { name="book" }
        { name="football" } ],
      "new hobbies should equal")


