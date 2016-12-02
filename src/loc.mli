type t =
  { start : Lexing.position
  ; stop  : Lexing.position
  }

val of_lexbuf : Lexing.lexbuf -> t

exception Error of t * string

val fail : t -> ('a, unit, string, _) format4 -> 'a
val fail_lex : Lexing.lexbuf -> ('a, unit, string, _) format4 -> 'a

val in_file : string -> t
