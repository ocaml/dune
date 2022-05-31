(** First line of versioned files *)

open Stdune

type t =
  { lang : Loc.t * string
  ; version : Loc.t * string
  }

(** Parse the first line of a versioned file. *)
val lex : Lexing.lexbuf -> t

(** Parse the first line of a versioned file but do not fail if it doesn't start
    with [(lang ...)]. *)
val lex_opt : Lexing.lexbuf -> t option
