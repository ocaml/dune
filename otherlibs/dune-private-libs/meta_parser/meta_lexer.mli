type token =
  | Name of string
  | String of string
  | Minus
  | Lparen
  | Rparen
  | Comma
  | Equal
  | Plus_equal
  | Eof

type user_error = { user_error : 'a. Lexing.lexbuf -> string -> 'a }

val token : user_error -> Lexing.lexbuf -> token
