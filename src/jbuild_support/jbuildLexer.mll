{
open Stdune
open Dune_lang

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

(* The difference between the old and new syntax is that the old
   syntax allows backslash following by any characters other than 'n',
   'x', ... and interpret it as it. The new syntax is stricter in
   order to allow introducing new escape sequence in the future if
   needed. *)
type escape_mode =
  | In_block_comment (* Inside #|...|# comments (old syntax) *)
  | In_quoted_string
}

let comment_body = [^ '\n' '\r']*
let comment   = ';' comment_body
let newline   = '\r'? '\n'
let blank     = [' ' '\t' '\012']
let digit     = ['0'-'9']
let hexdigit  = ['0'-'9' 'a'-'f' 'A'-'F']

let atom_char =
  [^ ';' '(' ')' '"' ' ' '\t' '\r' '\n' '\012']

(* rule for jbuild files *)
rule token with_comments = parse
  | newline
    { Lexing.new_line lexbuf; token with_comments lexbuf }
  | blank+
    { token with_comments lexbuf }
  | comment
    { if with_comments then
        comment_trail [Stdune.String.drop (Lexing.lexeme lexbuf) 1] lexbuf
      else
        token with_comments lexbuf
    }
  | '('
    { Lexer.Token.Lparen }
  | ')'
    { Rparen }
  | '"'
    { Buffer.clear escaped_buf;
      let start = Lexing.lexeme_start_p lexbuf in
      let s = quoted_string In_quoted_string lexbuf in
      lexbuf.lex_start_p <- start;
      Quoted_string s
    }
  | "#|"
    { let start = Lexing.lexeme_start_p lexbuf in
      block_comment lexbuf;
      if with_comments then begin
        lexbuf.lex_start_p <- start;
        Comment Legacy
      end else
        token false lexbuf
    }
  | "#;"
    { Sexp_comment }
  | eof
    { Eof }
  | ""
    { atom "" (Lexing.lexeme_start_p lexbuf) lexbuf }

and comment_trail acc = parse
  | newline blank* ';' (comment_body as s)
    { comment_trail (s :: acc) lexbuf }
  | ""
    { Lexer.Token.Comment (Lines (List.rev acc)) }

and atom acc start = parse
  | '#'+ '|'
    { lexbuf.lex_start_p <- start;
      error lexbuf "jbuild atoms cannot contain #|"
    }
  | '|'+ '#'
    { lexbuf.lex_start_p <- start;
      error lexbuf "jbuild atoms cannot contain |#"
    }
  | ('#'+ | '|'+ | (atom_char # ['|' '#'])) as s
    { atom (if acc = "" then s else acc ^ s) start lexbuf
    }
  | ""
    { if acc = "" then invalid_dune_or_jbuild lexbuf;
      lexbuf.lex_start_p <- start;
      Lexer.Token.Atom (Atom.of_string acc)
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
    { if mode = In_block_comment then
        error lexbuf "unterminated quoted string";
      Buffer.contents escaped_buf
    }

and quoted_string_after_escaped_newline mode = parse
  | [' ' '\t']*
    { quoted_string mode lexbuf }

and block_comment = parse
  | '"'
    { Buffer.clear escaped_buf;
      ignore (quoted_string In_block_comment lexbuf : string);
      block_comment lexbuf
    }
  | "|#"
    { ()
    }
  | eof
    { error lexbuf "unterminated block comment"
    }
  | _
    { block_comment lexbuf
    }

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
      if mode = In_quoted_string && v > 255 then
        error lexbuf "escape sequence in quoted string out of range"
          ~delta:(-1);
      Buffer.add_char escaped_buf (Char.chr v);
      Other
    }
  | digit* as s
    { if mode = In_quoted_string then
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
    { if mode = In_quoted_string then
        error lexbuf "unterminated hexadecimal escape sequence" ~delta:(-1);
      Buffer.add_char escaped_buf '\\';
      Buffer.add_string escaped_buf s;
      Other
    }
  | _ as c
    { Buffer.add_char escaped_buf '\\';
      Buffer.add_char escaped_buf c;
      Other
    }
  | eof
    { if mode = In_quoted_string then
        error lexbuf "unterminated escape sequence" ~delta:(-1);
      Other
    }

{
  let token ~with_comments lexbuf = token with_comments lexbuf
}
