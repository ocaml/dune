open! Stdune

type t

val of_string : string -> t
val to_string : t -> string
val equal : t -> t -> bool
val to_dyn : t -> Dyn.t
val encode : t Dune_sexp.Encoder.t
val decode : t Dune_sexp.Decoder.t
