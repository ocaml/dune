open! Stdune

module Token : sig
  type t =
    | Atom of Atom.t
    | Quoted_string of string
    | Lparen
    | Rparen
    | Eof
    | Template of Template.t
    | Comment of string list
    (** The following comment:

        {v
             ; abc
             ; def
        v}

        is represented as:

        {[
          Lines [ " abc"; " def" ]
        ]} *)
end

type t = with_comments:bool -> Lexing.lexbuf -> Token.t

val token : t
