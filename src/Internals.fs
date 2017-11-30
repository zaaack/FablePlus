module FablePlus.Internals


open System
open Fable.Core

[<Erase>]
type Default5 = class end
[<Erase>]
type Default4 = class inherit Default5 end
[<Erase>]
type Default3 = class inherit Default4 end
[<Erase>]
type Default2 = class inherit Default3 end
[<Erase>]
type Default1 = class inherit Default2 end
let inline flip f x y = f y x
let inline const' k _ = k
let inline either f g = function Choice2Of2 x -> f x | Choice1Of2 y -> g y
let inline option n f = function None -> n | Some x -> f x
let inline isNull (value : 'T) =  match value with null -> true | _ -> false
