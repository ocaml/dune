open! Stdune

type t =
  { name : Package_name.t
  ; constraint_ : Package_constraint.t option
  }

val encode : t Dune_sexp.Encoder.t
val decode : t Dune_sexp.Decoder.t
val to_dyn : t -> Dyn.t
val equal : t -> t -> bool
