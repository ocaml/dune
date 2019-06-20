(** Whether we are 'dune' or 'jbuilder' *)

type t = Dune | Jbuilder

val t : t
val dune2 : bool
