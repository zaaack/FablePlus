module FablePlus.Functor


open System.Runtime.CompilerServices
open System.Collections
open System.Collections.Generic
open System
open System.Runtime.InteropServices
open System.Text
// open Fable.Core
// open Fable.Core.JsInterop

// [<Erased>]
type Default5 = class end
type Default4 = class inherit Default5 end
type Default3 = class inherit Default4 end
type Default2 = class inherit Default3 end
type Default1 = class inherit Default2 end
let inline flip f x y = f y x
let inline const' k _ = k
let inline either f g = function Choice2Of2 x -> f x | Choice1Of2 y -> g y
let inline option n f = function None -> n | Some x -> f x
let inline isNull (value : 'T) =  match value with null -> true | _ -> false

// Monad class ------------------------------------------------------------

[<Extension;Sealed>]
type Bind =
    [<Extension>]static member Bind (source : Lazy<'T>    , f : 'T -> Lazy<'U>    ) = lazy (f source.Value).Value                                   : Lazy<'U>
    // [<Extension>]static member Bind (source : seq<'T>     , f : 'T -> seq<'U>     ) = Seq.bind f source                                             : seq<'U>
    [<Extension>]static member Bind (source               , f : 'T -> _           ) = Option.bind   f source                                        : option<'U>
    [<Extension>]static member Bind (source               , f : 'T -> _           ) = List.collect  f source                                        : list<'U>
    [<Extension>]static member Bind (source               , f : 'T -> _           ) = Array.collect f source                                        : 'U []
    [<Extension>]static member Bind (source               , k : 'T -> _           ) = (fun r -> k (source r) r)                                     : 'R->'U
    // static member inline       Bind ((w : 'Monoid, a : 'T), k : 'T -> 'Monoid * 'U) = let m, b = k a in (Plus.Invoke w m, b)                        : 'Monoid*'U
    [<Extension>]static member Bind (source               , f : 'T -> _           ) = async.Bind(source, f)                                         : Async<'U>
    // [<Extension>]static member Bind (source               , k : 'T -> _           ) = Error.bind k source                                           : Choice<'U,'E>

    [<Extension>]static member Bind (source : Map<'Key,'T>, f : 'T -> Map<'Key,'U>) = Map (seq {
                   for KeyValue(k, v) in source do
                       match Map.tryFind k (f v) with
                       | Some v -> yield k, v
                       | _      -> () })

    // [<Extension>]static member Bind (source : Dictionary<'Key,'T>, f : 'T -> Dictionary<'Key,'U>) =
    //                let d = Dictionary()
    //                for KeyValue(k, v) in source do
    //                    match (f v).TryGetValue(k)  with
    //                    | true, v -> d.Add(k, v)
    //                    | _       -> ()
    //                d

    // [<Extension>]static member Bind (source : ResizeArray<'T>, f : 'T -> ResizeArray<'U>) = ResizeArray(Seq.bind (f >> seq<_>) source)              : ResizeArray<'U>

    static member inline Invoke (source : '``Monad<'T>``) (binder : 'T -> '``Monad<'U>``) : '``Monad<'U>`` =
        let inline call (_mthd : 'M, input : 'I, _output : 'R, f) = ((^M or ^I or ^R) : (static member Bind: _*_ -> _) input, f)
        call (Unchecked.defaultof<Bind>, source, Unchecked.defaultof<'``Monad<'U>``>, binder)

    static member inline InvokeOnInstance (source : '``Monad<'T>``) (binder : 'T -> '``Monad<'U>``) : '``Monad<'U>`` =
        ((^``Monad<'T>`` or ^``Monad<'U>``) : (static member Bind: _*_ -> _) source, binder)


[<Extension;Sealed>]
type Map =
    inherit Default1
    static member inline Invoke (mapping :'T->'U) (source : '``Functor<'T>``) : '``Functor<'U>`` =
        let inline call (mthd : ^M, source : ^I, _output : ^R) = ((^M or ^I or ^R) : (static member Map: _*_*_ -> _) source, mapping, mthd)
        call (Unchecked.defaultof<Map>, source, Unchecked.defaultof<'``Functor<'U>``>)

    static member inline InvokeOnInstance (mapping :'T->'U) (source : '``Functor<'T>``) : '``Functor<'U>`` =
        (^``Functor<'T>`` : (static member Map: _ * _ -> _) source, mapping)

    // static member inline       Map (x : '``Monad<'T>``, f : 'T->'U) = Bind.InvokeOnInstance x (f >> Return.InvokeOnInstance) : '``Monad<'U>``
    // static member inline       Map (x : '``Applicative<'T>``, f : 'T->'U, [<OptionalArgument>]_impl:Default3) = Apply.InvokeOnInstance (Return.InvokeOnInstance f) x : '``Applicative<'U>``
    [<Extension>]static member Map (x : seq<_>, f : 'T->'U, [<Optional>]_impl:Default4) = Seq.map f x              : seq<'U>
    [<Extension>]static member Map (x : IDictionary<_,_>    , f : 'T->'U, [<Optional>]_impl:Default2) = let d = Dictionary() in Seq.iter (fun (KeyValue(k, v)) -> d.Add(k, f v)) x; d :> IDictionary<'Key,'U>
    [<Extension>]static member Map (x : IObservable<'T>     , f : 'T->'U, [<Optional>]_impl:Default2) = Observable.map f x       : IObservable<'U>
    static member inline       Map (x : '``Functor<'T>``    , f : 'T->'U, [<Optional>]_impl:Default1) = Map.InvokeOnInstance f x : '``Functor<'U>``

    [<Extension>]static member Map (x : Lazy<_>        , f : 'T->'U, [<Optional>]_mthd : Map) = Lazy<_>.Create (fun () -> f x.Value)   : Lazy<'U>
    [<Extension>]static member Map (x : option<_>      , f : 'T->'U, [<Optional>]_mthd : Map) = Option.map  f x
    [<Extension>]static member Map (x : Result<_, _>, f : 'T->'U, [<Optional>]_mthd : Map) = Result.map  f x
    [<Extension>]static member Map (x : list<_>        , f : 'T->'U, [<Optional>]_mthd : Map) = List.map f x                        : list<'U>
    [<Extension>]static member Map (g : 'R->'T         , f : 'T->'U, [<Optional>]_mthd : Map) = (>>) g f
    [<Extension>]static member Map (g : Func<'R, 'T>   , f : 'T->'U, [<Optional>]_mthd : Map) = Func<'R, 'U>(g.Invoke >> f)
    [<Extension>]static member Map ((m : 'Monoid, a)   , f : 'T->'U, [<Optional>]_mthd : Map) = (m, f a)
    [<Extension>]static member Map (x : _ []           , f : 'T->'U, [<Optional>]_mthd : Map) = Array.map   f x
    [<Extension>]static member Map (x : _ [,]          , f : 'T->'U, [<Optional>]_mthd : Map) = Array2D.map f x
    [<Extension>]static member Map (x : _ [,,]         , f : 'T->'U, [<Optional>]_mthd : Map) = Array3D.map f x
    [<Extension>]static member Map (x : _ [,,,]        , f : 'T->'U, [<Optional>]_mthd : Map) = Array4D.init (x.GetLength 0) (x.GetLength 1) (x.GetLength 2) (x.GetLength 3) (fun a b c d -> f x.[a,b,c,d])
    [<Extension>]static member Map (x : Async<_>       , f : 'T->'U, [<Optional>]_mthd : Map) = async.Bind(x, async.Return << f)
    // [<Extension>]static member Map (x : Choice<_,'E>   , f : 'T->'U, [<Optional>]_mthd : Map) = Error.map f x
    [<Extension>]static member Map (KeyValue(k, x)     , f : 'T->'U, [<Optional>]_mthd : Map) = KeyValuePair(k, f x)
    [<Extension>]static member Map (x : Map<'Key,'T>   , f : 'T->'U, [<Optional>]_mthd : Map) = Map.map (const' f) x : Map<'Key,'U>
    [<Extension>]static member Map (x : Dictionary<_,_>, f : 'T->'U, [<Optional>]_mthd : Map) = let d = Dictionary() in Seq.iter (fun (KeyValue(k, v)) -> d.Add(k, f v)) x; d: Dictionary<'Key,'U>
    // [<Extension>]static member Map (x : Expr<'T>       , f : 'T->'U, [<Optional>]_mthd : Map) = Expr.Cast<'U>(Expr.Application(Expr.Value(f),x))
    [<Extension>]static member Map (x : ResizeArray<'T>, f : 'T->'U, [<Optional>]_mthd : Map) = ResizeArray(Seq.map f x) : ResizeArray<'U>

    // Restricted
    [<Extension>]static member Map (x : string         , f, [<Optional>]_mthd : Map) = String.map f x
    [<Extension>]static member Map (x : StringBuilder  , f, [<Optional>]_mthd : Map) = StringBuilder(String.map f (x.ToString()))
    [<Extension>]static member Map (x : Set<_>         , f, [<Optional>]_mthd : Map) = Set.map f x

