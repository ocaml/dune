(** This module represents user defined bindings of the form (:foo bar). These
    are used in the dependency specification language for example *)

open Import

type 'a one =
  | Unnamed of 'a
  | Named of string * 'a list

type 'a t = 'a one list

val fold : 'a t -> f:('a one -> 'acc -> 'acc) -> init:'acc -> 'acc
val empty : 'a t
val to_list : 'a t -> 'a list
val singleton : 'a -> 'a t
val to_dyn : 'a Dyn.builder -> 'a t Dyn.builder
val decode : 'a Decoder.t -> 'a t Decoder.t
val encode : 'a Encoder.t -> 'a t -> Dune_sexp.t
val var_names : _ t -> string list
