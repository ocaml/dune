(** Left or right *)

(* CR-soon Alizter: When bumping the minimal version to 4.12, remove
   [Dune_either]. [Stdlib.Either] provides this natively. *)
module Dune_either : sig
  module Either : sig
    type ('a, 'b) t =
      | Left of 'a
      | Right of 'b
  end
end

include sig
  [@@@warning "-33"]

  (* This open is unused with OCaml >= 4.12 since the stdlib defines an either
     type. *)
  open Dune_either
  open Stdlib

  type ('a, 'b) t = ('a, 'b) Either.t =
    | Left of 'a
    | Right of 'b
end

val map : ('a, 'b) t -> l:('a -> 'c) -> r:('b -> 'c) -> 'c
val left : 'a -> ('a, _) t
val right : 'b -> (_, 'b) t
val to_dyn : ('a -> Dyn.t) -> ('b -> Dyn.t) -> ('a, 'b) t -> Dyn.t
