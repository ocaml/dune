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

let rec find_first_non_ascii s i =
  if i >= String.length s then None
  else if Char.code s.[i] >= 128 then Some i
  else find_first_non_ascii s (i + 1)
}

let newline   = '\r'? '\n'
let blank     = [' ' '\t']
let ascii = [ '\000' - '\127' ]
let atom_char = [^ ';' '(' ')' '"' '\000'-'\032' '\127'-'\255']
let atom_char_with_junk = [^';' '(' ')' '"' '#' '|' '\000'-'\032'] | [ '\127'-'\255' ]

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
  | atom_char_with_junk+ as s
    { let atom_start = Lexing.lexeme_start_p lexbuf in
      (* CR-someday benodiwal: Consider using Uuseg for more accurate grapheme
         cluster detection instead of the simple byte-based check. This would
         better handle complex Unicode like emoji sequences, though terminal
         rendering inconsistencies would still cause display issues. *)
      let loc_start, loc_end =
        match find_first_non_ascii s 0 with
        | None -> assert false
        | Some offset ->
          let error_start = { atom_start with pos_cnum = atom_start.pos_cnum + offset } in
          let error_end = { error_start with pos_cnum = error_start.pos_cnum + 1 } in
          error_start, error_end
      in
      (Loc.create ~start:loc_start ~stop:loc_end, s)
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
