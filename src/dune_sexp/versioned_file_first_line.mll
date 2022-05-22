{
open Stdune
type t =
  { lang    : Loc.t * string
  ; version : Loc.t * string
  }

let invalid_lang_line start lexbuf =
  lexbuf.Lexing.lex_start_p <- start;
  User_error.raise ~loc:(Loc.of_lexbuf lexbuf)
    [ Pp.text "Invalid first line, expected: (lang <lang> <version>)" ]
}

let newline   = '\r'? '\n'
let blank     = [' ' '\t']
let atom_char = [^';' '(' ')' '"' '#' '|' '\000'-'\032']

rule lex_opt = parse
  | '(' blank* "lang"
    { let start = Lexing.lexeme_start_p lexbuf in
      let lang    = atom start lexbuf in
      let version = atom start lexbuf in
      first_line_rparen_end start lexbuf;
      Some { lang; version }
    }
  | ""
    { None
    }

and atom start = parse
  | blank+
    { atom start lexbuf
    }
  | atom_char+ as s
    { (Loc.of_lexbuf lexbuf, s)
    }
  | _ | eof
    { to_eol lexbuf;
      invalid_lang_line start lexbuf
    }

and first_line_rparen_end start = parse
  | blank* ')' blank* (newline | eof as s)
    { if s <> "" then Lexing.new_line lexbuf
    }
  | ""
    { to_eol lexbuf;
      invalid_lang_line start lexbuf
    }

and to_eol = parse
  | [^'\r' '\n']*
    { ()
    }

{
  let lex lb =
    match lex_opt lb with
    | Some x -> x
    | None ->
      let start = Lexing.lexeme_start_p lb in
      to_eol lb;
      invalid_lang_line start lb
}
