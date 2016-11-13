(** META file parsing/printing *)

open! Import

type t =
  { name    : string
  ; entries : entry list
  }

and entry =
  | Comment of string
  | Var     of var
  | Package of t

and var = string * predicate list * action * string

and action = Set | Add

and predicate =
  | P of string (** Present *)
  | A of string (** Absent  *)

val load : string -> entry list

val flatten : t -> (string * var list) list
