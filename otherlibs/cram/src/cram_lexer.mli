(** .t file parser *)

(** A command or comment. Output blocks are skipped *)
type block =
  | Command of string list
  | Comment of string list

val block : Lexing.lexbuf -> block option
