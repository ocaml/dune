(** AST of the markdown for cmdliner doc *)

type t = item list

and item =
  | S   of string
  | P   of string
  | Pre of string
  | I   of string * string

