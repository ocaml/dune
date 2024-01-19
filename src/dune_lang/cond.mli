open! Stdune
open Dune_sexp

(** A cond statement *)
type t =
  { cases : (Blang.t * String_with_vars.t) list
  ; default : String_with_vars.t option
  ; loc : Loc.t
  }

val to_dyn : t -> Dyn.t
val decode : t Decoder.t
val equal : t -> t -> bool
