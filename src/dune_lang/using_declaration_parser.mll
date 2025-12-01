{
open Stdune

type using_decl =
  { name    : Loc.t * string
  ; version : Loc.t * string
  }
}

let newline   = '\r'? '\n'
let blank     = [' ' '\t']
(* Accept any non-whitespace, non-paren characters for version - including non-ASCII *)
let version_char = [^ ' ' '\t' '\r' '\n' '(' ')']

rule scan_all acc = parse
  | '(' blank* "using"
    { let start_pos = Lexing.lexeme_start_p lexbuf in
      match parse_using_decl start_pos lexbuf with
      | Some decl -> scan_all (decl :: acc) lexbuf
      | None -> scan_all acc lexbuf
    }
  | newline
    { Lexing.new_line lexbuf;
      scan_all acc lexbuf
    }
  | _
    { scan_all acc lexbuf
    }
  | eof
    { acc
    }

and parse_using_decl start_pos = parse
  | blank+
    { parse_using_decl start_pos lexbuf
    }
  | ([^ ' ' '\t' '\r' '\n' '(' ')']+ as name) blank+
    { let name_loc = Loc.of_lexbuf lexbuf in
      match parse_version start_pos lexbuf with
      | Some (ver_loc, ver) ->
        Some { name = (name_loc, name); version = (ver_loc, ver) }
      | None -> None
    }
  | _
    { skip_to_closing_paren lexbuf;
      None
    }

and parse_version start_pos = parse
  | blank+
    { parse_version start_pos lexbuf
    }
  | (version_char+ as ver) blank* ')'
    { let ver_loc = Loc.create
        ~start:lexbuf.Lexing.lex_start_p
        ~stop:{ lexbuf.Lexing.lex_curr_p with
                pos_cnum = lexbuf.Lexing.lex_curr_p.pos_cnum - 1 } in
      Some (ver_loc, ver)
    }
  | _
    { skip_to_closing_paren lexbuf;
      None
    }

and skip_to_closing_paren = parse
  | ')'
    { ()
    }
  | newline
    { Lexing.new_line lexbuf;
      skip_to_closing_paren lexbuf
    }
  | _
    { skip_to_closing_paren lexbuf
    }
  | eof
    { ()
    }

{
  (* Entry point: scan file and return all (using ...) declarations *)
  let scan contents fname =
    let lexbuf = Lexing.from_string contents in
    lexbuf.Lexing.lex_curr_p <- { lexbuf.Lexing.lex_curr_p with pos_fname = fname };
    scan_all [] lexbuf |> List.rev
}
