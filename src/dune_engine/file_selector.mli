(** A File_selector.t is a predicate that is to be evaluated in a particular
    directory *)

open Stdune

type t

val dir : t -> Path.t

val create : dir:Path.t -> string Predicate.t -> t

val equal : t -> t -> bool

val hash : t -> int

val compare : t -> t -> Ordering.t

val encode : t Dune_lang.Encoder.t

(** [to_dyn] is used as a marshallable representation of [t] (to compute
    digests), so it must be injective *)
val to_dyn : t -> Dyn.t

val test : t -> Path.t -> bool
