module FablePlus

open System
open System.Text.RegularExpressions
open Fable.Core
open Fable.Core.JsInterop


[<Import("setIn", "./Native.js")>]
let setIn: ('a -> 'b) -> 'b -> 'a -> 'a = fun _ _ a  -> a

