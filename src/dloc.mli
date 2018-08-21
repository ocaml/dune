open! Stdune

type t = Loc.t =
  { start : Lexing.position
  ; stop  : Lexing.position
  }

val exnf     : t             -> ('a, Format.formatter, unit, exn) format4 -> 'a
val fail     : t             -> ('a, Format.formatter, unit, 'b ) format4 -> 'a
val fail_lex : Lexing.lexbuf -> ('a, Format.formatter, unit, 'b ) format4 -> 'a
val fail_opt : t option      -> ('a, Format.formatter, unit, 'b ) format4 -> 'a

val in_file : string -> t

val none : t

(** Prints "File ..., line ..., characters ...:\n" *)
val print : Format.formatter -> t -> unit

(** Prints a warning *)
val warn : t -> ('a, Format.formatter, unit) format -> 'a
