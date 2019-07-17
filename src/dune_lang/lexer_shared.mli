module Token : sig
  module Comment : sig
    type t =
      | Lines of string list
      | Legacy
  end

  type t =
    | Atom          of Atom.t
    | Quoted_string of string
    | Lparen
    | Rparen
    | Sexp_comment
    | Eof
    | Template of Template.t
    | Comment of Comment.t
end

type t = with_comments:bool -> Lexing.lexbuf -> Token.t

val error : ?delta:int -> Lexing.lexbuf -> string -> 'a

val invalid_dune_or_jbuild : Lexing.lexbuf -> 'a

val escaped_buf : Buffer.t

type escape_sequence =
  | Newline
  | Other

val eval_decimal_char : char -> int

val eval_decimal_escape : char -> char -> char -> int

val eval_hex_escape : char -> char -> int
