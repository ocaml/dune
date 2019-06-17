open! Stdune

type t = Public | Private

include Dune_lang.Conv with type t := t

val is_public : t -> bool
val is_private : t -> bool

val to_sexp : t -> Sexp.t
val to_dyn : t -> Dyn.t

val pp : t Fmt.t
