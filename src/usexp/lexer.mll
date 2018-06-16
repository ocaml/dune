{
open Usexp0

module Token = struct
  type t =
    | Atom          of Atom.t
    | Quoted_string of string
    | Lparen
    | Rparen
    | Sexp_comment
    | Eof
    | Template      of Template.t
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

module Template = struct
  include Usexp0.Template

  let dummy_loc =
    { Loc.
      start = Lexing.dummy_pos
    ; stop = Lexing.dummy_pos
    }

  let add_text parts s =
    match parts with
    | Template.Text s' :: parts -> Template.Text (s' ^ s) :: parts
    | _ -> Template.Text s :: parts

  let token parts ~quoted ~start (lexbuf : Lexing.lexbuf) =
    match parts with
    | [] | [Text ""] ->
      lexbuf.lex_start_p <- start;
      error lexbuf "Internal error in the S-expression parser, \
                    please report upstream."
    | [Text s] ->
      lexbuf.lex_start_p <- start;
      Token.Atom (A s)
    | _ ->
      lexbuf.lex_start_p <- start;
      Token.Template
        { quoted
        ; loc = dummy_loc
        ; parts = List.rev parts
        }

  module Buffer = struct
    type state =
      | String
      | Template of Template.part list

    let text_buf = Buffer.create 256

    let take_buf () =
      let contents = Buffer.contents text_buf in
      Buffer.clear text_buf;
      contents

    let state = ref String

    let add_buf_to_parts parts =
      match take_buf () with
      | "" -> parts
      | t -> add_text parts t

    let get () =
      match !state with
      | String -> Token.Quoted_string (take_buf ())
      | Template parts ->
        state := String;
        begin match add_buf_to_parts parts with
        | [] -> assert false
        | [Text s] -> Quoted_string s
        | parts ->
          Token.Template
            { quoted = true
            ; loc = dummy_loc
            ; parts = List.rev parts
            }
        end

    let add_var v =
      match !state with
      | String ->
        state := Template (v :: add_buf_to_parts []);
      | Template parts ->
        let parts = add_buf_to_parts parts in
        state := Template (v::parts)

    let add_text   = Buffer.add_string text_buf
    let add_text_c = Buffer.add_char text_buf
  end
end

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

type block_string_line_kind =
  | With_escape_sequences
  | Raw
}

let comment   = ';' [^ '\n' '\r']*
let newline   = '\r'? '\n'
let blank     = [' ' '\t' '\012']
let atom_char = [^ ';' '(' ')' '"' ' ' '\t' '\r' '\n' '\012']
let digit     = ['0'-'9']
let hexdigit  = ['0'-'9' 'a'-'f' 'A'-'F']

let varname_char = atom_char # [ ':' '%' '$' '{' '}' ]

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
    { let start = Lexing.lexeme_start_p lexbuf in
      let token = quoted_string Old_syntax lexbuf in
      lexbuf.lex_start_p <- start;
      token
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
    { jbuild_atom [] (Lexing.lexeme_start_p lexbuf) lexbuf }

and template_variable syntax = parse
  | (varname_char+ as name) ':'? (varname_char* as payload) ([')' '}'] as close) {
    begin match syntax, close with
    | (Template.Percent | Template.Dollar_brace), '}'
    | Dollar_paren, ')' ->
      let (name, payload) =
        if payload = "" then
          ("", name)
        else
          (name, payload) in
      Template.Var
        { loc =
            { start = Lexing.lexeme_start_p lexbuf
            ; stop = Lexing.lexeme_end_p lexbuf
            }
        ; name
        ; payload
        ; syntax }
    | (Dollar_brace | Dollar_paren), _ ->
      error lexbuf (Printf.sprintf "Variable delimiters mismatched. \
                                    Ending delimiter '%c' is incorrect." close)
    | Percent, _ -> assert false
    end
  }
  | _ { error lexbuf "unexpected variable" }

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
    { jbuild_atom (Template.add_text acc s) start lexbuf }
  | "${" {
      jbuild_atom ((template_variable Dollar_brace lexbuf) :: acc) start lexbuf }
  | "" { Template.token acc ~quoted:false ~start lexbuf }

and quoted_string mode = parse
  | '"'
    { Template.Buffer.get () }
  | '\\'
    { match escape_sequence mode lexbuf with
      | Newline -> quoted_string_after_escaped_newline mode lexbuf
      | Other   -> quoted_string                       mode lexbuf
    }
  | (['$' '%'] as varchar) (['{' '('] as delim) {
      begin match varchar, delim, mode with
      | '%', '{', New_syntax
      | '$', _, Old_syntax ->
        let syntax =
          if varchar = '%' then
            Template.Percent
          else if delim = '{' then
            Dollar_brace
          else
            Dollar_paren in
        Template.Buffer.add_var (template_variable syntax lexbuf)
      | _, _, _ ->
        Template.Buffer.add_text_c varchar;
        Template.Buffer.add_text_c delim;
      end;
      quoted_string mode lexbuf
    }
  | newline as s
    { Lexing.new_line lexbuf;
      Template.Buffer.add_text s;
      quoted_string mode lexbuf
    }
  | _ as c
    { Template.Buffer.add_text_c c;
      quoted_string mode lexbuf
    }
  | eof
    { if mode <> In_block_comment then
        error lexbuf "unterminated quoted string";
      Template.Buffer.get ()
    }

and quoted_string_after_escaped_newline mode = parse
  | [' ' '\t']*
    { quoted_string mode lexbuf }

and escape_sequence mode = parse
  | newline
    { Lexing.new_line lexbuf;
      Newline }
  | ['\\' '\'' '"' '%' '$' 'n' 't' 'b' 'r'] as c
    { let c =
        match c with
        | 'n' -> '\n'
        | 'r' -> '\r'
        | 'b' -> '\b'
        | 't' -> '\t'
        | '$' when mode = New_syntax ->
          error lexbuf "unknown escape sequence" ~delta:(-2)
        | _   -> c
      in
      Template.Buffer.add_text_c c;
      Other
    }
  | (digit as c1) (digit as c2) (digit as c3)
    { let v = eval_decimal_escape c1 c2 c3 in
      if mode <> In_block_comment && v > 255 then
        error lexbuf "escape sequence in quoted string out of range"
          ~delta:(-1);
      Template.Buffer.add_text_c (Char.chr v);
      Other
    }
  | digit* as s
    { if mode <> In_block_comment then
        error lexbuf "unterminated decimal escape sequence" ~delta:(-1);
      Template.Buffer.add_text_c '\\';
      Template.Buffer.add_text s;
      Other
    }
  | 'x' (hexdigit as c1) (hexdigit as c2)
    { let v = eval_hex_escape c1 c2 in
      Template.Buffer.add_text_c (Char.chr v);
      Other
    }
  | 'x' hexdigit* as s
    { if mode <> In_block_comment then
        error lexbuf "unterminated hexadecimal escape sequence" ~delta:(-1);
      Template.Buffer.add_text_c '\\';
      Template.Buffer.add_text s;
      Other
    }
  | _ as c
    { if mode = New_syntax then
        error lexbuf "unknown escape sequence" ~delta:(-1);
      Template.Buffer.add_text_c '\\';
      Template.Buffer.add_text_c c;
      Other
    }
  | eof
    { if mode <> In_block_comment then
        error lexbuf "unterminated escape sequence" ~delta:(-1);
      Other
    }

and jbuild_block_comment = parse
  | '"'
    { ignore (quoted_string In_block_comment lexbuf : Token.t);
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

and dune_atom acc start = parse
  | atom_char+ as s { dune_atom (Template.add_text acc s) start lexbuf }
  | "%{" { dune_atom ((template_variable Percent lexbuf) :: acc) start lexbuf }
  | "" { Template.token acc ~quoted:false ~start lexbuf }

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
    { let start = Lexing.lexeme_start_p lexbuf in
      let token = dune_quoted_string lexbuf in
      lexbuf.lex_start_p <- start;
      token
    }
  | "" { dune_atom [] (Lexing.lexeme_start_p lexbuf) lexbuf }
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
      Template.Buffer.add_text s;
      block_string_after_newline lexbuf
    }
  | ' '
    { match kind with
      | With_escape_sequences -> block_string lexbuf
      | Raw -> raw_block_string lexbuf
    }
  | eof
    { Template.Buffer.get () }
  | _
    { error lexbuf "There must be at least one space after \"\\|"
    }

and block_string = parse
  | newline as s
    { Lexing.new_line lexbuf;
      Template.Buffer.add_text s;
      block_string_after_newline lexbuf
    }
  | '\\'
    { match escape_sequence New_syntax lexbuf with
      | Newline -> block_string_after_newline lexbuf
      | Other   -> block_string               lexbuf
    }
  | "%{" {
      let var = template_variable Percent lexbuf in
      Template.Buffer.add_var var;
      block_string lexbuf
    }
  | _ as c
    { Template.Buffer.add_text_c c;
      block_string lexbuf
    }
  | eof
    { Template.Buffer.get ()
    }

and block_string_after_newline = parse
  | blank* "\"\\|"
    { block_string_start With_escape_sequences lexbuf }
  | blank* "\"\\>"
    { block_string_start Raw lexbuf }
  | ""
    { Template.Buffer.get ()
    }

and raw_block_string = parse
  | newline as s
    { Lexing.new_line lexbuf;
      Template.Buffer.add_text s;
      block_string_after_newline lexbuf
    }
  | _ as c
    { Template.Buffer.add_text_c c;
      raw_block_string lexbuf
    }
  | eof
    { Template.Buffer.get ()
    }
