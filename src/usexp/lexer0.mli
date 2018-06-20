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

module Error : sig
  type t =
    { start   : Lexing.position
    ; stop    : Lexing.position
    ; message : string
    }
end

val error : ?delta:int -> Lexing.lexbuf -> string -> 'a

val escaped_buf : Buffer.t

exception Error of Error.t

type escape_mode =
  | In_block_comment (* Inside #|...|# comments (old syntax) *)
  | Old_syntax
  | New_syntax

type escape_sequence =
  | Newline
  | Other

type block_string_line_kind =
  | With_escape_sequences
  | Raw

val eval_decimal_char : char -> int

val eval_decimal_escape : char -> char -> char -> int

val eval_hex_escape : char -> char -> int
