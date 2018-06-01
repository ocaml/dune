module Atom : sig
  type t = A of string [@@unboxed]
end

module Token : sig
  type t =
    | Atom          of Atom.t
    | Quoted_string of string
    | Lparen
    | Rparen
    | Sexp_comment
    | Eof
end

type t = Lexing.lexbuf -> Token.t

val token : t

module Error : sig
  type t =
    { start   : Lexing.position
    ; stop    : Lexing.position
    ; message : string
    }
end

exception Error of Error.t
