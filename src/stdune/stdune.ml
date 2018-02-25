module Ansi_color = Ansi_color
module Array      = Array
module Comparable = Comparable
module Either     = Either
module Exn        = Exn
module Filename   = Filename
module Hashtbl    = Hashtbl
module Int        = Int
module List       = List
module Map        = Map
module Option     = Option
module Ordering   = Ordering
module Pp         = Pp
module Result     = Result
module Set        = Set
module Staged     = Staged
module String     = String

external reraise : exn -> _ = "%reraise"

let compare a b = Ordering.of_int (compare a b)

(* The following types are re-exported here so that they are always
   available in scope *)

type ('a, 'error) result = ('a, 'error) Result.t =
  | Ok    of 'a
  | Error of 'error

type ('a, 'b) either = ('a, 'b) Either.t =
  | Left  of 'a
  | Right of 'b

type ordering = Ordering.t =
  | Lt
  | Eq
  | Gt
