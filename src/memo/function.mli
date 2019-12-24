open Stdune

(* Why are function names represented as interned strings? Most (or all) of
   function names are string literals and are therefore already interned by the
   compiler, which means we do not get any extra memory saving.

   The answer is that function names are used as keys in memoization tables, and
   by interning them manually we can map them into a contiguous range of
   integers, which in turn allows the memoization tables to be plain arrays. *)
module Name : Interned.S

module Type : sig
  type ('a, 'b, 'f) t =
    | Sync : ('a, 'b, 'a -> 'b) t
    | Async : ('a, 'b, 'a -> 'b Fiber.t) t
end

module Info : sig
  type t =
    { name : Name.t
    ; doc : string option
    }
end

type ('a, 'b, 'f) t =
  | Sync : ('a -> 'b) -> ('a, 'b, 'a -> 'b) t
  | Async : ('a -> 'b Fiber.t) -> ('a, 'b, 'a -> 'b Fiber.t) t

val of_type : ('a, 'b, 'c) Type.t -> 'c -> ('a, 'b, 'c) t
