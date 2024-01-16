open! Stdune
open Dune_sexp

type t =
  { conditions : (Blang.t * String_with_vars.t) list
  ; loc : Loc.t
  }

val to_dyn : t -> Dyn.t
val decode : t Decoder.t
val equal : t -> t -> bool
