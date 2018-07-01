type t =
  { start : Lexing.position
  ; stop  : Lexing.position
  }

val in_file : string -> t

val none : t

val of_lexbuf : Lexing.lexbuf -> t
