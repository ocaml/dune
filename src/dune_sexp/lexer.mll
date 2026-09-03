{
open Stdune

module Token = struct
  type t =
    | Atom of Atom.t
    | Quoted_string of string
    | Block_string of Block_string.t
    | Lparen
    | Rparen
    | Eof
    | Template of Template.t
    | Comment of string list
end

type t = with_comments:bool -> Lexing.lexbuf -> Token.t

let error ?(delta = 0) lexbuf message =
  let start = Lexing.lexeme_start_p lexbuf in
  let loc =
    Loc.create ~start:{ start with pos_cnum = start.pos_cnum + delta }
      ~stop:(Lexing.lexeme_end_p lexbuf)
  in
  User_error.raise ~loc [ Pp.text message ]

let invalid_dune_or_jbuild lexbuf =
  let start = Lexing.lexeme_start_p lexbuf in
  let fname = Filename.basename start.pos_fname in
  error lexbuf (sprintf "Invalid %s file" fname)

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

module Template = struct
  include Template

  let dummy_loc = Loc.create ~start:Lexing.dummy_pos ~stop:Lexing.dummy_pos

  let add_text parts s =
    match (parts : Part.t list)  with
    | Text s' :: parts -> Part.Text (s' ^ s) :: parts
    | _ -> Text s :: parts

  let token parts ~quoted ~start (lexbuf : Lexing.lexbuf) =
    lexbuf.lex_start_p <- start;
    match (parts : Part.t list) with
    | [] | [Text ""] ->
      invalid_dune_or_jbuild lexbuf
    | [Text s] ->
      Token.Atom (Atom.of_string_unchecked s)
    | _ ->
      Token.Template
        { quoted
        ; loc = dummy_loc
        ; parts = List.rev parts
        }

  module Buffer : sig
    val new_token : unit -> unit
    val get : unit -> Token.t
    val add_var : Part.t -> unit
    val add_text : string -> unit
    val add_text_c : char -> unit
    val add_char : char -> unit
    val add_newline : string -> unit
    val begin_block : Block_string.Kind.t -> unit
    val start_block_line : Block_string.Kind.t -> unit
  end = struct
    type state =
      | String
      | Template of Part.t list
      | Block_string of
          { current_kind : Block_string.Kind.t
          ; current_line_started : bool
          ; current_line_parts : Part.t list
          ; lines : Block_string.t
          }

    let text_buf = Buffer.create 256
    let state = ref String

    let new_token () =
      Buffer.clear text_buf;
      state := String
    ;;

    let take_buf () =
      let contents = Buffer.contents text_buf in
      Buffer.clear text_buf;
      contents
    ;;

    let add_buf_to_parts parts =
      match take_buf () with
      | "" -> parts
      | text -> add_text parts text
    ;;

    let begin_block kind =
      state
      := Block_string
           { current_kind = kind
           ; current_line_started = true
           ; current_line_parts = []
           ; lines = []
           }
    ;;

    let start_block_line kind =
      match !state with
      | Block_string ({ current_line_started = false; _ } as block) ->
        state
        := Block_string { block with current_kind = kind; current_line_started = true }
      | String | Template _ | Block_string _ -> ()
    ;;

    let finish_block_line () =
      match !state with
      | Block_string { current_kind; current_line_parts; lines; _ } ->
        let parts = add_buf_to_parts current_line_parts in
        let line = current_kind, List.rev parts in
        state
        := Block_string
             { current_kind
             ; current_line_started = false
             ; current_line_parts = []
             ; lines = line :: lines
             }
      | String | Template _ -> Buffer.add_char text_buf '\n'
    ;;

    let add_text text = Buffer.add_string text_buf text
    let add_text_c char = Buffer.add_char text_buf char

    let add_char char =
      match !state, char with
      | Block_string _, '\n' -> finish_block_line ()
      | Block_string block, _ ->
        Buffer.add_char text_buf char;
        if not block.current_line_started
        then state := Block_string { block with current_line_started = true }
      | (String | Template _), _ -> Buffer.add_char text_buf char
    ;;

    let add_newline newline =
      match !state with
      | Block_string _ -> String.iter newline ~f:add_char
      | String | Template _ -> add_text newline
    ;;

    let get () =
      match !state with
      | String -> Token.Quoted_string (take_buf ())
      | Block_string { current_kind; current_line_parts; lines; _ } ->
        let final_parts = add_buf_to_parts current_line_parts in
        let final_line = current_kind, List.rev final_parts in
        state := String;
        Token.Block_string (List.rev (final_line :: lines))
      | Template parts ->
        state := String;
        (match add_buf_to_parts parts with
         | [] -> assert false
         | [ Text s ] -> Quoted_string s
         | parts ->
           Token.Template
             { quoted = true
             ; loc = dummy_loc
             ; parts = List.rev parts
             })
    ;;

    let add_var var =
      match !state with
      | String -> state := Template (var :: add_buf_to_parts [])
      | Block_string block ->
        let current_line_parts = add_buf_to_parts block.current_line_parts in
        state
        := Block_string
             { block with
               current_line_started = true
             ; current_line_parts = var :: current_line_parts
             }
      | Template parts ->
        let parts = add_buf_to_parts parts in
        state := Template (var :: parts)
    ;;
  end
end
}

let comment_body = [^ '\n' '\r']*
let comment   = ';' comment_body
let newline   = '\r'? '\n'
let blank     = [' ' '\t' '\012']
let digit     = ['0'-'9']
let hexdigit  = ['0'-'9' 'a'-'f' 'A'-'F']

let atom_char = [^ ';' '(' ')' '"' '\000'-'\032' '\127'-'\255']
let varname_char = atom_char # [ ':' '%' '{' '}' ]

rule token with_comments = parse
  | newline
    { Lexing.new_line lexbuf; token with_comments lexbuf }
  | blank+
    { token with_comments lexbuf }
  | comment
    { if with_comments then
        comment_trail [String.drop (Lexing.lexeme lexbuf) 1] lexbuf
      else
        token with_comments lexbuf
    }
  | '('
    { Token.Lparen }
  | ')'
    { Rparen }
  | '"'
    { let start = Lexing.lexeme_start_p lexbuf in
      Template.Buffer.new_token ();
      let token = start_quoted_string with_comments lexbuf in
      lexbuf.lex_start_p <- start;
      token
    }
  | eof
    { Eof }
  | ""
    { atom [] (Lexing.lexeme_start_p lexbuf) lexbuf }

and comment_trail acc = parse
  | newline blank* ';' (comment_body as s)
    { comment_trail (s :: acc) lexbuf }
  | ""
    { Token.Comment (List.rev acc) }

and atom acc start = parse
  | (atom_char # '%')+ as s
    { atom (Template.add_text acc s) start lexbuf }
  | "%{"
    { atom ((template_variable lexbuf) :: acc) start lexbuf }
  | "%"
    { atom (Template.add_text acc "%") start lexbuf }
  | ['\128'-'\255']
    { error lexbuf Atom.non_ascii_error_message }
  | ""
    { Template.token acc ~quoted:false ~start lexbuf }

and start_quoted_string preserve_block_syntax = parse
  | "\\|"
    { if preserve_block_syntax
      then Template.Buffer.begin_block Block_string.Kind.Escaped;
      block_string_start Block_string.Kind.Escaped lexbuf
    }
  | "\\>"
    { if preserve_block_syntax
      then Template.Buffer.begin_block Block_string.Kind.Raw;
      block_string_start Block_string.Kind.Raw lexbuf
    }
  | ""
    { quoted_string lexbuf }

and block_string_start kind = parse
  | newline as newline
    { Lexing.new_line lexbuf;
      Template.Buffer.add_newline newline;
      block_string_after_newline lexbuf
    }
  | ' '
    { match kind with
      | Block_string.Kind.Escaped -> block_string lexbuf
      | Raw -> raw_block_string lexbuf
    }
  | eof
    { Template.Buffer.get () }
  | _
    { error lexbuf "There must be at least one space after \"\\|"
    }

and block_string = parse
  | newline as newline
    { Lexing.new_line lexbuf;
      Template.Buffer.add_newline newline;
      block_string_after_newline lexbuf
    }
  | '\\'
    { match escape_sequence lexbuf with
      | Newline -> block_string_after_newline lexbuf
      | Other -> block_string lexbuf
    }
  | "%{"
    { Template.Buffer.add_var (template_variable lexbuf);
      block_string lexbuf
    }
  | _ as char
    { Template.Buffer.add_char char;
      block_string lexbuf
    }
  | eof
    { Template.Buffer.get ()
    }

and block_string_after_newline = parse
  | blank* "\"\\|"
    { Template.Buffer.start_block_line Block_string.Kind.Escaped;
      block_string_start Block_string.Kind.Escaped lexbuf
    }
  | blank* "\"\\>"
    { Template.Buffer.start_block_line Block_string.Kind.Raw;
      block_string_start Block_string.Kind.Raw lexbuf
    }
  | ""
    { Template.Buffer.get ()
    }

and raw_block_string = parse
  | newline as newline
    { Lexing.new_line lexbuf;
      Template.Buffer.add_newline newline;
      block_string_after_newline lexbuf
    }
  | _ as char
    { Template.Buffer.add_char char;
      raw_block_string lexbuf
    }
  | eof
    { Template.Buffer.get ()
    }

and quoted_string = parse
  | '"'
    { Template.Buffer.get () }
  | '\\'
    { match escape_sequence lexbuf with
      | Newline -> quoted_string_after_escaped_newline lexbuf
      | Other   -> quoted_string                       lexbuf
    }
  | "%{"
    { Template.Buffer.add_var (template_variable lexbuf);
      quoted_string lexbuf
    }
  | newline as s
    { Lexing.new_line lexbuf;
      Template.Buffer.add_text s;
      quoted_string lexbuf
    }
  | _ as c
    { Template.Buffer.add_text_c c;
      quoted_string lexbuf
    }
  | eof
    { error lexbuf "unterminated quoted string"
    }

and escape_sequence = parse
  | newline
    { Lexing.new_line lexbuf;
      Newline }
  | '%'
    { Template.Buffer.add_char '%';
      Other
    }
  | ['\\' '\'' '"' 'n' 't' 'b' 'r'] as c
    { let c =
        match c with
        | 'n' -> '\n'
        | 'r' -> '\r'
        | 'b' -> '\b'
        | 't' -> '\t'
        | _   -> c
      in
      Template.Buffer.add_char c;
      Other
    }
  | (digit as c1) (digit as c2) (digit as c3)
    { let v = eval_decimal_escape c1 c2 c3 in
      if v > 255 then
        error lexbuf "escape sequence in quoted string out of range"
          ~delta:(-1);
      Template.Buffer.add_char (Char.chr v);
      Other
    }
  | digit digit digit
    { error lexbuf "escape sequence in quoted string out of range" ~delta:(-1);
    }
  | digit digit?
    { error lexbuf "unterminated decimal escape sequence" ~delta:(-1);
    }
  | 'x' (hexdigit as c1) (hexdigit as c2)
    { let v = eval_hex_escape c1 c2 in
      Template.Buffer.add_char (Char.chr v);
      Other
    }
  | 'x' hexdigit?
    { error lexbuf "unterminated hexadecimal escape sequence" ~delta:(-1);
    }
  | _
    { error lexbuf "unknown escape sequence" ~delta:(-1);
    }
  | eof
    { error lexbuf "unterminated escape sequence" ~delta:(-1);
    }

and quoted_string_after_escaped_newline = parse
  | [' ' '\t']*
    { quoted_string lexbuf }

and template_variable = parse
  | (varname_char+ as name) (':' ((':' | varname_char)* as payload))? '}'
    { let payload =
        match payload with
        | Some "" -> error lexbuf "payload after : in variable cannot be empty"
        | p -> p
      in
      let start = Lexing.lexeme_start_p lexbuf in
      (* -2 to account for the "%{" *)
      let start = { start with pos_cnum = start.pos_cnum - 2 } in
      let loc = Loc.create ~start ~stop:(Lexing.lexeme_end_p lexbuf) in
      Template.Part.Pform
        { loc ; name ; payload = Option.map ~f:Template.Pform.Payload.of_string payload }
  }
  | '}' | eof
    { error lexbuf "%{...} forms cannot be empty" }
  | (varname_char* as skip) (_ as other)
  | (varname_char+ ':' ((':' | varname_char)*) as skip) (_ as other)
  {
    error
      ~delta:(String.length skip)
      lexbuf
      (Printf.sprintf "The character %C is not allowed inside %%{...} forms" other)
  }

{
  let token ~with_comments lexbuf = token with_comments lexbuf
}
