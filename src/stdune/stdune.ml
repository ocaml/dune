module Ansi_color = Ansi_color
module Array      = Array
module Int        = Int
module List       = List
module Map        = Map
module Option     = Option
module Ordering   = Ordering
module Result     = Result
module Set        = Set
module String     = String

external reraise : exn -> _ = "%reraise"

let compare a b = Ordering.of_int (compare a b)
