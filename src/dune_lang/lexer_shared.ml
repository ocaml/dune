open Stdune

module Token = struct
  module Comment = struct
    type t =
      | Lines of string list
      | Legacy

    let to_dyn =
      let open Dyn.Encoder in
      function
      | Legacy -> constr "Legacy" []
      | Lines l -> constr "Lines" [ list string l ]
  end

  type t =
    | Atom of Atom.t
    | Quoted_string of string
    | Lparen
    | Rparen
    | Sexp_comment
    | Eof
    | Template of Template.t
    | Comment of Comment.t
end

type t = with_comments:bool -> Lexing.lexbuf -> Token.t

let error ?(delta = 0) lexbuf message =
  let start = Lexing.lexeme_start_p lexbuf in
  let loc : Loc.t =
    { start = { start with pos_cnum = start.pos_cnum + delta }
    ; stop = Lexing.lexeme_end_p lexbuf
    }
  in
  User_error.raise ~loc [ Pp.text message ]

let invalid_dune_or_jbuild lexbuf =
  let start = Lexing.lexeme_start_p lexbuf in
  let fname = Filename.basename start.pos_fname in
  error lexbuf (sprintf "Invalid %s file" fname)

let escaped_buf = Buffer.create 256

type escape_sequence =
  | Newline
  | Other

let eval_decimal_char c = Char.code c - Char.code '0'

let eval_decimal_escape c1 c2 c3 =
  (eval_decimal_char c1 * 100)
  + (eval_decimal_char c2 * 10)
  + eval_decimal_char c3

let eval_hex_char c =
  match c with
  | '0' .. '9' -> Char.code c - Char.code '0'
  | 'a' .. 'f' -> Char.code c - Char.code 'a' + 10
  | 'A' .. 'F' -> Char.code c - Char.code 'A' + 10
  | _ -> -1

let eval_hex_escape c1 c2 = (eval_hex_char c1 * 16) + eval_hex_char c2
