{
  open Lexer0
}

let comment   = ';' [^ '\n' '\r']*
let newline   = '\r'? '\n'
let blank     = [' ' '\t' '\012']
let digit     = ['0'-'9']
let hexdigit  = ['0'-'9' 'a'-'f' 'A'-'F']

let atom_char_jbuild =
  [^ ';' '(' ')' '"' ' ' '\t' '\r' '\n' '\012']

(* rule for jbuild files *)
rule token = parse
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
      let s = quoted_string Old_syntax lexbuf in
      lexbuf.lex_start_p <- start;
      Quoted_string s
    }
  | "#|"
    { jbuild_block_comment lexbuf;
      token lexbuf
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
  | ('#'+ | '|'+ | (atom_char_jbuild # ['|' '#'])) as s
    { jbuild_atom (if acc = "" then s else acc ^ s) start lexbuf
    }
  | ""
    { if acc = "" then
        error lexbuf "Internal error in the S-expression parser, \
                      please report upstream.";
      lexbuf.lex_start_p <- start;
      Token.Atom (Atom.of_string acc)
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
