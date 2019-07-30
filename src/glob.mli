open! Stdune

type t

val equal : t -> t -> bool

val hash : t -> int

val to_dyn : t Dyn.Encoder.t

val decode : t Dune_lang.Decoder.t

val test : t -> string -> bool

val filter : t -> string list -> string list

val empty : t

val of_string_exn : Loc.t -> string -> t

val to_pred : t -> string Predicate.t
