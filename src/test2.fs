
module xxx
open Fable.Core
open System
open System.Runtime.InteropServices
open Fable.Import
open System.Text
type A<'T>() =
  member x.aa = 1

type Map =
    static member inline Invoke (mapping :'T->'U) (source : '``Functor<'T>``) : '``Functor<'U>`` =
        let inline call (mthd : ^M, source : ^I, _output : ^R) = ((^M or ^I or ^R) : (static member Map: _*_*_ -> _) source, mapping, mthd)
        call (Unchecked.defaultof<Map>, source, Unchecked.defaultof<'``Functor<'U>``>)
    static member Map (x : option<_>      , f : 'T->'U, [<Optional>]_mthd : Map) = Option.map  f x
    static member Map (x : Result<_, _>, f : 'T->'U, [<Optional>]_mthd : Map) = Result.map  f x
    static member Map (x : Async<_>, f : 'T->'U, [<Optional>]_mthd : Map) = async.Bind(x, f >> async.Return)
    static member Map (x : Lazy<_>        , f : 'T->'U, [<Optional>]_mthd : Map) = Lazy<_>.Create (fun () -> f x.Value)   : Lazy<'U>
    static member Map (x : 'T JS.Promise, f : 'T->'U, [<Optional>]_mthd : Map) = x
    static member Map (x : StringBuilder, f : 'a->'b, [<Optional>]_mthd : Map) = x
    static member Map (x : 'a A, f : 'a->'b, [<Optional>]_mthd : Map) = x


/// Lift a function into a Functor.
let inline map    (f:'T->'U) (x:'``Functor<'T>``) :'``Functor<'U>`` = Map.Invoke f x

/// Lift a function into a Functor. Same as map.
let inline (<!>)  (f:'T->'U) (x:'``Functor<'T>``) :'``Functor<'U>`` = Map.Invoke f x

/// Lift a function into a Functor. Same as map.
let inline (<<|)  (f:'T->'U) (x:'``Functor<'T>``) :'``Functor<'U>`` = Map.Invoke f x

/// Lift a function into a Functor. Same as map but with flipped arguments.
let inline (|>>)  (x:'``Functor<'T>``) (f:'T->'U) :'``Functor<'U>`` = Map.Invoke f x
let a = Some 1

a |>> (fun a -> a + 1) |> printfn  "some: %A"

let b = Ok 1

b |>> (fun a -> a + 1) |> printfn  "ok: %A"
