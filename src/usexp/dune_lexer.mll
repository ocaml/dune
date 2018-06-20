{
  open Lexer0
}

let comment   = ';' [^ '\n' '\r']*
let newline   = '\r'? '\n'
let blank     = [' ' '\t' '\012']
let digit     = ['0'-'9']
let hexdigit  = ['0'-'9' 'a'-'f' 'A'-'F']

let atom_char_dune =
  [^ '%' ';' '(' ')' '"' '\000'-'\032' '\127'-'\255']

(* rule for dune files *)
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
      let s = dune_quoted_string lexbuf in
      lexbuf.lex_start_p <- start;
      Quoted_string s
    }
  | atom_char_dune+ as s
    { Token.Atom (Atom.of_string s) }
  | _ as c { error lexbuf (Printf.sprintf "Invalid atom character '%c'" c) }
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

and quoted_string_after_escaped_newline mode = parse
  | [' ' '\t']*
    { quoted_string mode lexbuf }
