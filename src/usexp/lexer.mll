{
module Atom = struct
  type t = A of string [@@unboxed]
end

module Token = struct
  type t =
    | Atom          of Atom.t
    | Quoted_string of string
    | Lparen
    | Rparen
    | Sexp_comment
    | Eof
end

type t = Lexing.lexbuf -> Token.t

module Error = struct
  type t =
    { start   : Lexing.position
    ; stop    : Lexing.position
    ; message : string
    }
end

exception Error of Error.t

let error ?(delta=0) lexbuf message =
  let start = Lexing.lexeme_start_p lexbuf in
  raise
    (Error { start = { start with pos_cnum = start.pos_cnum + delta }
           ; stop  = Lexing.lexeme_end_p   lexbuf
           ; message
           })

(* The difference between the old and new syntax is that the old
   syntax allows backslash following by any characters other than 'n',
   'x', ... and interpret it as it. The new syntax is stricter in
   order to allow introducing new escape sequence in the future if
   needed. *)
type escape_mode =
  | In_block_comment (* Inside #|...|# comments (old syntax) *)
  | Old_syntax
  | New_syntax

let eval_decimal_char c = Char.code c - Char.code '0'

let eval_decimal_escape c1 c2 c3 =
  (eval_decimal_char c1) * 100 +
  (eval_decimal_char c2) * 10  +
  (eval_decimal_char c3)

let eval_hex_char c =
  match c with
  | '0'..'9' -> Char.code c - Char.code '0'
  | 'a'..'f' -> Char.code c - Char.code 'a' + 10
  | 'A'..'F' -> Char.code c - Char.code 'A' + 10
  | _ -> -1

let eval_hex_escape c1 c2 =
  (eval_hex_char c1) * 16 +
  (eval_hex_char c2)

type escape_sequence =
  | Newline
  | Other

let escaped_buf = Buffer.create 256

type block_string_line_kind =
  | With_escape_sequences
  | Raw
}

let comment   = ';' [^ '\n' '\r']*
let newline   = '\r'? '\n'
let blank     = [' ' '\t' '\012']
let atom_char =
  [^ '%' ';' '(' ')' '"' ' ' '\t' '\r' '\n' '\000'-'\032' '\127'-'\255']
let digit     = ['0'-'9']
let hexdigit  = ['0'-'9' 'a'-'f' 'A'-'F']

(* rule for jbuild files *)
rule jbuild_token = parse
  | newline
    { Lexing.new_line lexbuf; jbuild_token lexbuf }
  | blank+ | comment
    { jbuild_token lexbuf }
  | '('
    { Token.Lparen }
  | ')'
    { Rparen }
  | '"'
    { Buffer.clear escaped_buf;
      let start = Lexing.lexeme_start_p lexbuf in
      let s = quoted_string Old_syntax lexbuf in
      lexbuf.lex_start_p <- start;
      Quoted_string s
    }
  | "#|"
    { jbuild_block_comment lexbuf;
      jbuild_token lexbuf
    }
  | "#;"
    { Sexp_comment }
  | eof
    { Eof }
  | ""
    { jbuild_atom "" (Lexing.lexeme_start_p lexbuf) lexbuf }

and jbuild_atom acc start = parse
  | '#'+ '|'
    { lexbuf.lex_start_p <- start;
      error lexbuf "jbuild_atoms cannot contain #|"
    }
  | '|'+ '#'
    { lexbuf.lex_start_p <- start;
      error lexbuf "jbuild_atoms cannot contain |#"
    }
  | ('#'+ | '|'+ | (atom_char # ['|' '#'])) as s
    { jbuild_atom (if acc = "" then s else acc ^ s) start lexbuf
    }
  | ""
    { if acc = "" then
        error lexbuf "Internal error in the S-expression parser, \
                      please report upstream.";
      lexbuf.lex_start_p <- start;
      Token.Atom (A acc)
    }

and quoted_string mode = parse
  | '"'
    { Buffer.contents escaped_buf }
  | '\\'
    { match escape_sequence mode lexbuf with
      | Newline -> quoted_string_after_escaped_newline mode lexbuf
      | Other   -> quoted_string                       mode lexbuf
    }
  | newline as s
    { Lexing.new_line lexbuf;
      Buffer.add_string escaped_buf s;
      quoted_string mode lexbuf
    }
  | _ as c
    { Buffer.add_char escaped_buf c;
      quoted_string mode lexbuf
    }
  | eof
    { if mode <> In_block_comment then
        error lexbuf "unterminated quoted string";
      Buffer.contents escaped_buf
    }

and quoted_string_after_escaped_newline mode = parse
  | [' ' '\t']*
    { quoted_string mode lexbuf }

and escape_sequence mode = parse
  | newline
    { Lexing.new_line lexbuf;
      Newline }
  | ['\\' '\'' '"' 'n' 't' 'b' 'r'] as c
    { let c =
        match c with
        | 'n' -> '\n'
        | 'r' -> '\r'
        | 'b' -> '\b'
        | 't' -> '\t'
        | _   -> c
      in
      Buffer.add_char escaped_buf c;
      Other
    }
  | (digit as c1) (digit as c2) (digit as c3)
    { let v = eval_decimal_escape c1 c2 c3 in
      if mode <> In_block_comment && v > 255 then
        error lexbuf "escape sequence in quoted string out of range"
          ~delta:(-1);
      Buffer.add_char escaped_buf (Char.chr v);
      Other
    }
  | digit* as s
    { if mode <> In_block_comment then
        error lexbuf "unterminated decimal escape sequence" ~delta:(-1);
      Buffer.add_char escaped_buf '\\';
      Buffer.add_string escaped_buf s;
      Other
    }
  | 'x' (hexdigit as c1) (hexdigit as c2)
    { let v = eval_hex_escape c1 c2 in
      Buffer.add_char escaped_buf (Char.chr v);
      Other
    }
  | 'x' hexdigit* as s
    { if mode <> In_block_comment then
        error lexbuf "unterminated hexadecimal escape sequence" ~delta:(-1);
      Buffer.add_char escaped_buf '\\';
      Buffer.add_string escaped_buf s;
      Other
    }
  | _ as c
    { if mode = New_syntax then
        error lexbuf "unknown escape sequence" ~delta:(-1);
      Buffer.add_char escaped_buf '\\';
      Buffer.add_char escaped_buf c;
      Other
    }
  | eof
    { if mode <> In_block_comment then
        error lexbuf "unterminated escape sequence" ~delta:(-1);
      Other
    }

and jbuild_block_comment = parse
  | '"'
    { Buffer.clear escaped_buf;
      ignore (quoted_string In_block_comment lexbuf : string);
      jbuild_block_comment lexbuf
    }
  | "|#"
    { ()
    }
  | eof
    { error lexbuf "unterminated block comment"
    }
  | _
    { jbuild_block_comment lexbuf
    }

(* rule for dune files *)
and token = parse
  | newline
    { Lexing.new_line lexbuf; token lexbuf }
  | blank+ | comment
    { token lexbuf }
  | '('
    { Token.Lparen }
  | ')'
    { Rparen }
  | '"'
    { Buffer.clear escaped_buf;
      let start = Lexing.lexeme_start_p lexbuf in
      let s = dune_quoted_string lexbuf in
      lexbuf.lex_start_p <- start;
      Quoted_string s
    }
  | atom_char+ as s
    { Token.Atom (A s) }
  | eof
    { Eof }

and dune_quoted_string = parse
  | "\\|"
    { block_string_start With_escape_sequences lexbuf }
  | "\\>"
    { block_string_start Raw lexbuf }
  | ""
    { quoted_string New_syntax lexbuf }

and block_string_start kind = parse
  | newline as s
    { Lexing.new_line lexbuf;
      Buffer.add_string escaped_buf s;
      block_string_after_newline lexbuf
    }
  | ' '
    { match kind with
      | With_escape_sequences -> block_string lexbuf
      | Raw -> raw_block_string lexbuf
    }
  | eof
    { Buffer.contents escaped_buf
    }
  | _
    { error lexbuf "There must be at least one space after \"\\|"
    }

and block_string = parse
  | newline as s
    { Lexing.new_line lexbuf;
      Buffer.add_string escaped_buf s;
      block_string_after_newline lexbuf
    }
  | '\\'
    { match escape_sequence New_syntax lexbuf with
      | Newline -> block_string_after_newline lexbuf
      | Other   -> block_string               lexbuf
    }
  | _ as c
    { Buffer.add_char escaped_buf c;
      block_string lexbuf
    }
  | eof
    { Buffer.contents escaped_buf
    }

and block_string_after_newline = parse
  | blank* "\"\\|"
    { block_string_start With_escape_sequences lexbuf }
  | blank* "\"\\>"
    { block_string_start Raw lexbuf }
  | ""
    { Buffer.contents escaped_buf
    }

and raw_block_string = parse
  | newline as s
    { Lexing.new_line lexbuf;
      Buffer.add_string escaped_buf s;
      block_string_after_newline lexbuf
    }
  | _ as c
    { Buffer.add_char escaped_buf c;
      raw_block_string lexbuf
    }
  | eof
    { Buffer.contents escaped_buf
    }
