(** Returns [true] if the input starts with "(* -*- tuareg -*- *)" *)
val is_script : Lexing.lexbuf -> bool

type first_line =
  { lang    : Loc.t * string
  ; version : Loc.t * string
  }

(** Parse the first line of a dune-project file. *)
val first_line : Lexing.lexbuf -> first_line
