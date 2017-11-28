module FablePlus.Tests

open System
open System.Text.RegularExpressions
open Fable.Core
open Fable.Core.JsInterop
open Fable.Core.Testing


open Fable.Import
open Fable.Import.Node
open Fable.Import
open Fable.PowerPack

[<Emit("describe($0, $1)")>]
let describe (str: string) (fn: unit -> unit) = jsNative

[<Emit("it($0, $1)")>]
let it (str: string) (fn: unit -> unit) = jsNative

[<Emit("it($0, $1)")>]
let itAsync (str: string) (fn: unit -> JS.Promise<unit>) = jsNative


exception ShouldThrowException of string

describe "test rules" <| fun () ->
  it "should Trim work" <| fun () -> ()
