(** Defines build profile for dune. Only one profile is active per context. Some
    profiles are treated specially by dune. *)

type t =
  | Dev
  | Release
  | User_defined of string

val equal : t -> t -> bool
val is_dev : t -> bool
val is_release : t -> bool
val is_inline_test : t -> bool

include Dune_util.Stringlike with type t := t

val default : t
