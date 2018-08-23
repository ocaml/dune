open! Stdune

type t = Loc.t =
  { start : Lexing.position
  ; stop  : Lexing.position
  }

val in_file : string -> t

val none : t

(** Prints "File ..., line ..., characters ...:\n" *)
val print : Format.formatter -> t -> unit

(** Prints a warning *)
val warn : t -> ('a, Format.formatter, unit) format -> 'a
