open! Stdune

val is_script : Lexing.lexbuf -> bool
(** Returns [true] if the input starts with "(* -*- tuareg -*- *)" *)

val eof_reached : Lexing.lexbuf -> bool
