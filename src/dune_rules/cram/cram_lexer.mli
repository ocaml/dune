open Import

(** .t file parser *)

(** A command or comment. Output blocks are skipped *)
type 'command block =
  | Command of 'command
  | Comment of string list

val block : Lexing.lexbuf -> (Loc.t * string list block) option
