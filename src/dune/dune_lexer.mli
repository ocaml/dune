open! Stdune

(** Returns [true] if the input starts with "(* -*- tuareg -*- *)" *)
val is_script : Lexing.lexbuf -> bool

val eof_reached : Lexing.lexbuf -> bool
