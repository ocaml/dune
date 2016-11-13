type t =
  { start : Lexing.position
  ; stop  : Lexing.position
  }

val of_lexbuf : Lexing.lexbuf -> t

exception Error of t * string

val fail : t -> string -> _
val fail_lex : Lexing.lexbuf -> string -> _
