module FablePlus.Operators

open Functor


    // Common combinators
let inline flip f x y = f y x
let inline konst k _ = k
let inline curry f x y = f (x, y)
let inline uncurry f (x, y) = f x y
let inline (</) x = (|>) x
let inline (/>) x = flip x
let inline either f g = function Choice2Of2 x -> f x | Choice1Of2 y -> g y
let inline option n f = function None -> n | Some x -> f x
let inline tuple2 a b             = a,b
let inline tuple3 a b c           = a,b,c
let inline tuple4 a b c d         = a,b,c,d
let inline tuple5 a b c d e       = a,b,c,d,e
let inline tuple6 a b c d e f     = a,b,c,d,e,f
let inline tuple7 a b c d e f g   = a,b,c,d,e,f,g
let inline tuple8 a b c d e f g h = a,b,c,d,e,f,g,h
// BEGIN region copied from FsControl

// Functor ----------------------------------------------------------------

/// Lift a function into a Functor.
let inline map    (f:'T->'U) (x:'``Functor<'T>``) :'``Functor<'U>`` = Map.Invoke f x

/// Lift a function into a Functor. Same as map.
let inline (<!>)  (f:'T->'U) (x:'``Functor<'T>``) :'``Functor<'U>`` = Map.Invoke f x

/// Lift a function into a Functor. Same as map.
let inline (<<|)  (f:'T->'U) (x:'``Functor<'T>``) :'``Functor<'U>`` = Map.Invoke f x

/// Lift a function into a Functor. Same as map but with flipped arguments.
let inline (|>>)  (x:'``Functor<'T>``) (f:'T->'U) :'``Functor<'U>`` = Map.Invoke f x


let a: Result<int, string> = Ok 1

let c = a |>> fun b -> b + 2
