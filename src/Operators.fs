[<AutoOpen>]
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


// // Applicative ------------------------------------------------------------

// /// Lift a value into a Functor. Same as return in Computation Expressions.
// // let inline result (x:'T): '``Functor<'T>`` = Return.Invoke x

// /// Apply a lifted argument to a lifted function.
// let inline (<*>) (x:'``Applicative<'T -> 'U>``) (y:'``Applicative<'T>``): '``Applicative<'U>`` = Apply.Invoke x y : '``Applicative<'U>``

// /// Apply 2 lifted arguments to a lifted function.
// let inline liftA2 (f:'T->'U->'V) (a:'``Applicative<'T>``) (b:'``Applicative<'U>``) : '``Applicative<'V>`` = f <!> a <*> b

// let inline (  *>)   (x:'``Applicative<'T>``) : '``Applicative<'U>``->'``Applicative<'U>`` = x |> liftA2 (fun   _ -> id)
// let inline (<*  )   (x:'``Applicative<'T>``) : '``Applicative<'U>``->'``Applicative<'T>`` = x |> liftA2 (fun k _ -> k )
// let inline (<**>)   (x:'``Applicative<'T>``) : '``Applicative<'T -> 'U>``->'``Applicative<'U>`` = x |> liftA2 (|>)
// // let inline optional (v:'``Applicative<'T>``) : '``Applicative<Option'T>`` = Some <!> v <|> result None




// Monad -----------------------------------------------------------

/// Takes a monadic value and a function from a plain type to a monadic value, and returns a new monadic value.
let inline (>>=) (x:'``Monad<'T>``) (f:'T->'``Monad<'U>``) :'``Monad<'U>`` = Bind.Invoke x f

/// Takes a function from a plain type to a monadic value and a monadic value, and returns a new monadic value.
let inline (=<<) (f:'T->'``Monad<'U>``) (x:'``Monad<'T>``) :'``Monad<'U>`` = Bind.Invoke x f

let inline (>=>) (f:'T->'``Monad<'U>``) (g:'U->'``Monad<'V>``) (x:'T) : '``Monad<'V>`` = Bind.Invoke (f x) g
let inline (<=<) (g:'b->'``Monad<'V>``) (f:'T->'``Monad<'U>``) (x:'T) : '``Monad<'V>`` = Bind.Invoke (f x) g

/// Flattens two layers of monadic information into one.
// let inline join  (x:'``Monad<Monad<'T>>``) : '``Monad<'T>`` = Join.Invoke x
