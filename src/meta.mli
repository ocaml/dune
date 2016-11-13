(** META file parsing/printing *)

open! Import

type t =
  { name    : string
  ; entries : entry list
  }

and entry =
  | Comment of string
  | Var     of string * predicate list * action * string
  | Package of t

and action = Set | Add

and predicate =
  | P of string (** Present *)
  | A of string (** Absent  *)

val load : string -> entry list
