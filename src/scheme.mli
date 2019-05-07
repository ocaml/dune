(** A collection of rules for one or multiple directories. *)

open! Stdune

type 'rules t =
  | Empty
  | Union of 'rules t * 'rules t
  | Approximation of Dir_set.t * 'rules t
  | Finite of 'rules Path.Build.Map.t
  | Thunk of (unit -> 'rules t)

module Evaluated : sig
  type 'a t

  val get_rules : 'a t -> dir:Path.Build.t -> 'a option
end

val evaluate : 'a t -> union:('a -> 'a -> 'a) -> 'a Evaluated.t

val all : 'a t list -> 'a t
