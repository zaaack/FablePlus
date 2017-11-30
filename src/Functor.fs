module FablePlus.Functor


open System.Runtime.CompilerServices
open System.Collections
open System.Collections.Generic
open System
open System.Runtime.InteropServices
open System.Text
open Extensions
open Internals
open Fable.Core
open Fable.Core.JsInterop
open Fable.PowerPack.PromiseImpl
open Fable.Import

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

// [<Extension;Sealed>]
// type Join =
//     inherit Default1
//     // static member       Join (x : '``Monad<'Monad<'T>>``, [<Optional>]_output : '``Monad<'T>``  , [<Optional>]_impl : Default2) = Bind.InvokeOnInstance x id: '``Monad<'T>``
//     // static member inline       Join (x : '``Monad<'Monad<'T>>``, [<Optional>]_output : '``Monad<'T>``  , [<Optional>]_impl : Default1) = ((^``Monad<'Monad<'T>>`` or  ^``Monad<'T>``) : (static member Join: _ -> _) x) : '``Monad<'T>``
//     static member Join (x : Lazy<Lazy<_>>         , [<Optional>]_output : Lazy<'T>        , [<Optional>]_impl : Join    ) = lazy x.Value.Value        : Lazy<'T>
//     // [<Extension>]static member Join (x                         , [<Optional>]_output : seq<'T>         , [<Optional>]_impl : Join    ) = Seq.bind id x             : seq<'T>
//     // [<Extension>]static member Join (x : Id<_>                 , [<Optional>]_output : Id<'T>          , [<Optional>]_impl : Join    ) = x.getValue                : Id<'T>
//     [<Extension>]static member Join (x                         , [<Optional>]_output : option<'T>      , [<Optional>]_impl : Join    ) = Option.bind   id x        : option<'T>
//     // [<Extension>]static member Join (x                         , [<Optional>]_output : list<'T>        , [<Optional>]_impl : Join    ) = List.collect  id x        : list<'T>
//     // [<Extension>]static member Join (x                         , [<Optional>]_output : 'T []           , [<Optional>]_impl : Join    ) = Array.collect id x        : 'T []
//     [<Extension>]static member Join (g                         , [<Optional>]_output : 'R->'T          , [<Optional>]_impl : Join    ) = (fun r -> (g r) r)        : 'R->'T
//     // static member inline       Join (m1, (m2, x)               , [<Optional>]_output : 'Monoid * 'T    , [<Optional>]_impl : Join    ) = Plus.Invoke m1 m2, x      : 'Monoid*'T
//     [<Extension>]static member Join (x                         , [<Optional>]_output : Async<'T>       , [<Optional>]_impl : Join    ) = async.Bind(x, id)         : Async<'T>
//     [<Extension>]static member Join (x                         , [<Optional>]_output : JS.Promise<'T>       , [<Optional>]_impl : Join    ) = promise.Bind(x, id)         : JS.Promise<'T>
//     [<Extension>]static member Join (x                         , [<Optional>]_output : Choice<'T,'E>   , [<Optional>]_impl : Join    ) = Choice2.bind id x           : Choice<'T,'E>

//     // [<Extension>]static member Join (x : Map<_,_>                     , [<Optional>]_output : Map<'Key,'Value>, [<Optional>]_impl : Join    )                             : Map<'Key,'Value> =
//     //                 Map (seq {
//     //                     for KeyValue(k, v) in x do
//     //                         match Map.tryFind k v with
//     //                         | Some v -> yield k, v
//     //                         | _      -> () })

//     // [<Extension>]static member Join (x : Dictionary<_,Dictionary<_,_>>, [<Optional>]_output : Dictionary<'Key,'Value>, [<Optional>]_impl:Join)                            : Dictionary<'Key,'Value> =
//     //                 let d = Dictionary()
//     //                 for KeyValue(k, v) in x do
//     //                     match v.TryGetValue(k)  with
//     //                     | true, v -> d.Add(k, v)
//     //                     | _       -> ()
//     //                 d

//     // [<Extension>]static member Join (x:ResizeArray<ResizeArray<'T>>   , [<Optional>]_output : ResizeArray<'T>        , [<Optional>]_impl:Join) = ResizeArray(Seq.bind seq<_> x) : ResizeArray<'T>

//     static member inline Invoke (source : '``Monad<Monad<'T>>``) : '``Monad<'T>`` =
//         let inline call (mthd : 'M, input : 'I, output : 'R) = ((^M or ^I or ^R) : (static member Join: _*_*_ -> _) input, output, mthd)
//         call (Unchecked.defaultof<Join>, source, Unchecked.defaultof<'``Monad<'T>``>)


// // type Return =
// //     inherit Default1

// //     static member Invoke (x:'T) : '``Applicative<'T>`` =
// //         let inline call (mthd : ^M, output : ^R) = ((^M or ^R) : (static member Return: _*_ -> _) output, mthd)
// //         call (Unchecked.defaultof<Return>, Unchecked.defaultof<'``Applicative<'T>``>) x
// //     static member Return (_:option<'a>    , _:Return) = fun x -> Some x      :option<'a>
// //     // static member inline InvokeOnInstance (x:'T) = (^``Applicative<'T>`` : (static member Return: ^T -> ^``Applicative<'T>``) x)

// //     // static member Return (_:seq<'a> , _:Default2) = fun  x     -> Seq.singleton x :seq<'a>
// //     // static member Return (_:'R      , _:Default1) = fun (x:'T) -> Return.InvokeOnInstance x :'R

// //     static member Return (_:Lazy<'a>, _:Return) = fun x -> Lazy<_>.CreateFromValue x : Lazy<'a>
// //     static member Return (_:Result<'T, 'TError>    , _:Return) = fun x -> Ok x      :Result<'T, 'TError>
// //     // static member Return (_:list<'a>      , _:Return) = fun x -> [ x ]       :list<'a>
// //     // static member Return (_:'a []         , _:Return) = fun x -> [|x|]       :'a []
// //     // static member Return (_:'r -> 'a      , _:Return) = const':'a  -> 'r -> _
// //     static member Return (_:'a Async      , _:Return) = fun (x:'a) -> async.Return x
// //     static member Return (_:JS.Promise<'T>, _:Return) = fun (x:'T) -> promise.Return x
// //     static member Return (_:Choice<'a,'e> , _:Return) = fun x -> Choice1Of2 x :Choice<'a,'e>
// //     // static member Return (_:'a ResizeArray, _:Return) = fun x -> ResizeArray<'a>(Seq.singleton x)

// //     // //Restricted
// //     // static member Return (_:string       , _:Return) = fun (x:char) -> string x : string
// //     // static member Return (_:StringBuilder, _:Return) = fun (x:char) -> new StringBuilder(string x):StringBuilder
// //     // static member Return (_:'a Set       , _:Return) = fun (x:'a  ) -> Set.singleton x

// type Apply =
//     inherit Default1
//     static member inline Invoke (f:'``Applicative<'T -> 'U>``) (x:'``Applicative<'T>``) : '``Applicative<'U>`` =
//         let inline call (mthd : ^M, input1 : ^I1, input2 : ^I2, output : ^R) =
//             ((^M or ^I1 or ^I2 or ^R) : (static member ``<\*>`` : _*_*_*_ -> _) input1, input2, output, mthd)
//         call(Unchecked.defaultof<Apply>, f, x, Unchecked.defaultof<'``Applicative<'U>``>)
//     // static member ``<\*>`` (f:'``Applicative<'T->'U>``, x:'``Applicative<'T>``, [<Optional>]_output:'``Applicative<'U>``, [<Optional>]_impl:Default1) : '``Applicative<'U>`` = ((^``Applicative<'T->'U>`` or ^``Applicative<'T>`` or ^``Applicative<'U>``) : (static member (<*>): _*_ -> _) f, x)

// //     static member ``<\*>`` (f:Lazy<'T->'U>, x:Lazy<'T>       , [<Optional>]_output:Lazy<'U>     , [<Optional>]_impl:Apply) = Lazy<_>.Create (fun () -> f.Value x.Value) : Lazy<'U>
// //     static member ``<\*>`` (f:seq<_>      , x:seq<'T>        , [<Optional>]_output:seq<'U>      , [<Optional>]_impl:Apply) = Seq.apply  f x :seq<'U>
// //     static member ``<\*>`` (f:list<_>     , x:list<'T>       , [<Optional>]_output:list<'U>     , [<Optional>]_impl:Apply) = List.apply f x :list<'U>
// //     static member ``<\*>`` (f:_ []        , x:'T []          , [<Optional>]_output:'U []        , [<Optional>]_impl:Apply) = Array.collect (fun x1 -> Array.collect (fun x2 -> [|x1 x2|]) x) f :'U []
// //     static member ``<\*>`` (f:'r -> _     , g: _ -> 'T       , [<Optional>]_output: 'r -> 'U    , [<Optional>]_impl:Apply) = fun x -> f x (g x) :'U
// //     // static member ``<\*>`` ((a:'Monoid, f), (b:'Monoid, x:'T), [<Optional>]_output:'Monoid * 'U , [<Optional>]_impl:Apply) = (Plus.Invoke a b, f x) :'Monoid *'U
//     static member ``<\*>`` (f:Async<_>    , x:Async<'T>      , [<Optional>]_output:Async<'U>    , [<Optional>]_impl:Apply) = async.Bind (f, fun x1 -> async.Bind (x, fun x2 -> async {return x1 x2})) :Async<'U>
//     // static member ``<\*>`` (f:JS.Promise<_>    , x:JS.Promise<'T>      , [<Optional>]_output:JS.Promise<'U>    , [<Optional>]_impl:Apply) = promise.Bind (f, fun x1 -> promise.Bind (x, fun x2 -> promise {return x1 x2}))
//     static member ``<\*>`` (f:option<_>   , x:option<'T>     , [<Optional>]_output:option<'U>   , [<Optional>]_impl:Apply) = Option.apply f x    :option<'U>
//     static member ``<\*>`` (f:Choice<_,'E>, x:Choice<'T,'E>  , [<Optional>]_output:Choice<'b,'E>, [<Optional>]_impl:Apply) = Choice2.apply f x :Choice<'U,'E>
// //     // static member ``<\*>`` (KeyValue(a:'Key, f), KeyValue(b:'Key, x:'T), [<Optional>]_output:KeyValuePair<'Key,'U>, [<Optional>]_impl:Apply) :KeyValuePair<'Key,'U> = KeyValuePair(Plus.Invoke a b, f x)

// //     static member ``<\*>`` (f:Map<'Key,_>      , x:Map<'Key,'T>        , [<Optional>]_output:Map<'Key,'U>, [<Optional>]_impl:Apply) :Map<'Key,'U> = Map (seq {
// //        for KeyValue(k, vf) in f do
// //            match Map.tryFind k x with
// //            | Some vx -> yield k, vf vx
// //            | _       -> () })

// //     static member ``<\*>`` (f:Dictionary<'Key,_>, x:Dictionary<'Key,'T>, [<Optional>]_output:Dictionary<'Key,'U>, [<Optional>]_impl:Apply) :Dictionary<'Key,'U> =
// //        let d = Dictionary()
// //        for KeyValue(k, vf) in f do
// //            match x.TryGetValue k with
// //            | true, vx -> d.Add(k, vf vx)
// //            | _        -> ()
// //        d

// //     // static member ``<\*>`` (f:Expr<'T->'U>, x:Expr<'T>, [<Optional>]_output:Expr<'U>, [<Optional>]_impl:Apply) = Expr.Cast<'U>(Expr.Application(f,x))

// //     static member ``<\*>`` (f:('T->'U) ResizeArray, x:'T ResizeArray, [<Optional>]_output:'U ResizeArray, [<Optional>]_impl:Apply) =
// //        ResizeArray(Seq.collect (fun x1 -> Seq.collect (fun x2 -> Seq.singleton (x1 x2)) x) f) :'U ResizeArray

//     // static member inline InvokeOnInstance (f:'``Applicative<'T->'U>``) (x:'``Applicative<'T>``) : '``Applicative<'U>`` =
//         // ((^``Applicative<'T->'U>`` or ^``Applicative<'T>`` or ^``Applicative<'U>``) : (static member (<*>): _*_ -> _) (f, x))

//     // static member ``<\*>`` (f:'``Monad<'T->'U>``  , x:'``Monad<'T>``  , [<Optional>]_output:'``Monad<'U>``  , [<Optional>]_impl:Default2) : '``Monad<'U>``   = Bind.InvokeOnInstance f (fun (x1:'T->'U) -> Bind.InvokeOnInstance x (fun x2 -> Return.Invoke(x1 x2)))

// Monad class ------------------------------------------------------------

type Bind =
    static member inline Invoke (source : '``Monad<'T>``) (binder : 'T -> '``Monad<'U>``) : '``Monad<'U>`` =
        let inline call (_mthd : 'M, input : 'I, _output : 'R, f) = ((^M or ^I or ^R) : (static member Bind: _*_ -> _) input, f)
        call (Unchecked.defaultof<Bind>, source, Unchecked.defaultof<'``Monad<'U>``>, binder)
    static member Bind (source : Lazy<'T>    , f : 'T -> Lazy<'U>    ) = lazy (f source.Value).Value                                   : Lazy<'U>
    // [<Extension>]static member Bind (source : seq<'T>     , f : 'T -> seq<'U>     ) = Seq.bind f source                                             : seq<'U>
    static member Bind (source               , f : 'T -> _           ) = Option.bind   f source                                        : option<'U>
    static member Bind (source               , f : 'T -> _           ) = Result.bind   f source                                        : Result<'U, 'TError>
    // [<Extension>]static member Bind (source               , f : 'T -> _           ) = List.collect  f source                                        : list<'U>
    // [<Extension>]static member Bind (source               , f : 'T -> _           ) = Array.collect f source                                        : 'U []
    static member Bind (source               , k : 'T -> _           ) = (fun r -> k (source r) r)                                     : 'R->'U
    // static member inline       Bind ((w : 'Monoid, a : 'T), k : 'T -> 'Monoid * 'U) = let m, b = k a in (Plus.Invoke w m, b)                        : 'Monoid*'U
    static member Bind (source               , f : 'T -> _           ) = async.Bind(source, f)                                         : Async<'U>
    static member Bind (source               , f : 'T -> _           ) = promise.Bind(source, f)                                         : JS.Promise<'U>
    static member Bind (source               , k : 'T -> _           ) = Choice2.bind k source                                           : Choice<'U,'E>

    // [<Extension>]static member Bind (source : Map<'Key,'T>, f : 'T -> Map<'Key,'U>) = Map (seq {
    //                for KeyValue(k, v) in source do
    //                    match Map.tryFind k (f v) with
    //                    | Some v -> yield k, v
    //                    | _      -> () })

    // [<Extension>]static member Bind (source : Dictionary<'Key,'T>, f : 'T -> Dictionary<'Key,'U>) =
    //                let d = Dictionary()
    //                for KeyValue(k, v) in source do
    //                    match (f v).TryGetValue(k)  with
    //                    | true, v -> d.Add(k, v)
    //                    | _       -> ()
    //                d

    // [<Extension>]static member Bind (source : ResizeArray<'T>, f : 'T -> ResizeArray<'U>) = ResizeArray(Seq.bind (f >> seq<_>) source)              : ResizeArray<'U>

    // static member inline InvokeOnInstance (source : '``Monad<'T>``) (binder : 'T -> '``Monad<'U>``) : '``Monad<'U>`` =
    //     ((^``Monad<'T>`` or ^``Monad<'U>``) : (static member Bind: _*_ -> _) source, binder)

type Map =
    static member inline Invoke (mapping :'T->'U) (source : '``Functor<'T>``) : '``Functor<'U>`` =
        let inline call (mthd : ^M, source : ^I, _output : ^R) = ((^M or ^I or ^R) : (static member Map: _*_*_ -> _) source, mapping, mthd)
        call (Unchecked.defaultof<Map>, source, Unchecked.defaultof<'``Functor<'U>``>)

    // static member inline InvokeOnInstance (f:'``Functor<'T->'U>``) (x:'``Functor<'T>``) : '``Functor<'U>`` =
    //     ((^``Functor<'T->'U>`` or ^``Functor<'T>`` or ^``Functor<'U>``) : (static member Map: _*_ -> _) (f, x))
    // static member inline       Map (x : '``Functor<'T>``    , f : 'T->'U, [<Optional>]_impl:Default1) = Map.InvokeOnInstance f x : '``Functor<'U>``
    static member Map (x : 'R -> 'T, f : 'T->'U, [<Optional>]_mthd : Map) = x >> f
    static member Map (x : option<_>      , f : 'T->'U, [<Optional>]_mthd : Map) = Option.map  f x
    static member Map (x : Result<_, _>, f : 'T->'U, [<Optional>]_mthd : Map) = Result.map  f x
    static member Map (x : Async<_>, f : 'T->'U, [<Optional>]_mthd : Map) = async.Bind(x, f >> async.Return)
    // static member Map (x : seq<_>, f : 'T->'U, [<Optional>]_mthd : Map) = Seq.map f x
    static member Map (x : Lazy<_>        , f : 'T->'U, [<Optional>]_mthd : Map) = Lazy<_>.Create (fun () -> f x.Value)   : Lazy<'U>
    static member Map (x : 'T JS.Promise, f : 'T->'U, [<Optional>]_mthd : Map) = promise.Bind(x, f >> promise.Return)
    static member Map (x : Choice<_, _>, f : 'a->'b, [<Optional>]_mthd : Map) = Choice2.map f x
