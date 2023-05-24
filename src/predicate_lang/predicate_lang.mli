(** DSL to define sets that are defined by a membership : 'a -> bool function. *)

open! Stdune
open Dune_sexp

type 'a t =
  | Element of 'a
  | Compl of 'a t
  | Standard
  | Union of 'a t list
  | Inter of 'a t list

val diff : 'a t -> 'a t -> 'a t

val inter : 'a t list -> 'a t

val compl : 'a t -> 'a t

val union : 'a t list -> 'a t

val not_union : 'a t list -> 'a t

val any : 'a t

val decode_one : 'a Decoder.t -> 'a t Decoder.t

val decode : 'a Decoder.t -> 'a t Decoder.t

val encode : 'a Encoder.t -> 'a t Encoder.t

val to_dyn : 'a Dyn.builder -> 'a t Dyn.builder

val exec : 'a t -> standard:'a t -> ('a -> bool) -> bool

val empty : 'a t

val map : 'a t -> f:('a -> 'b) -> 'b t

val to_predicate :
  'a Predicate.t t -> standard:'a Predicate.t t -> 'a Predicate.t

val compare : ('a -> 'a -> Ordering.t) -> 'a t -> 'a t -> Ordering.t

module Glob : sig
  type nonrec t = Dune_glob.V1.t t

  val to_dyn : t -> Dyn.t

  val exec : t -> standard:t -> string -> bool

  val filter : t -> standard:t -> string list -> string list

  val of_glob : Dune_glob.V1.t -> t

  val of_string_list : string list -> t

  val of_string_set : String.Set.t -> t

  val compare : t -> t -> Ordering.t

  val hash : t -> int

  val encode : t -> Dune_sexp.t
end
