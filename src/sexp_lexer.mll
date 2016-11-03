{
type stack =
  | Empty
  | Open of Lexing.position * stack
  | Sexp of Sexp.t * Sexp.Locs.t * stack

exception Parse_error of Lexing.position * string
let error lexbuf msg =
  raise (Parse_error (Lexing.lexeme_start_p lexbuf, msg))

let make_list =
  let rec loop lexbuf acc acc_locs = function
    | Empty ->
      error lexbuf "right parenthesis without matching left parenthesis"
    | Open (start, stack) ->
      Sexp (List acc,
            List ({ start; stop = Lexing.lexeme_end_p lexbuf }, acc_locs),
            stack)
    | Sexp (sexp, locs, stack) -> loop lexbuf (sexp :: acc) (locs :: acc_locs) stack
  in
  fun lexbuf stack -> loop lexbuf [] [] stack

let new_sexp loop stack lexbuf =
  match stack with
  | Sexp (sexp, locs, Empty) -> Some (sexp, locs)
  | _ -> loop stack lexbuf

let atom_loc lexbuf : Sexp.Locs.t =
  Atom
    { start = Lexing.lexeme_start_p lexbuf
    ; stop  = Lexing.lexeme_end_p   lexbuf
    }
}

let lf = '\010'
let lf_cr = ['\010' '\013']
let dos_newline = "\013\010"
let blank = [' ' '\009' '\012']
let unquoted = [^ ';' '(' ')' '"'] # blank # lf_cr

rule main stack = parse
  | lf | dos_newline
    { Lexing.new_line lexbuf; main stack lexbuf }
  | blank+
    { main stack lexbuf }
  | (';' (_ # lf_cr)*)
    { main stack lexbuf }
  | '('
    { main (Open (Lexing.lexeme_start_p lexbuf, stack)) lexbuf }
  | ')'
    { new_sexp main (make_list lexbuf stack) lexbuf }
  | '"' (("\\" _ | [^'"'])* as s) '"'
    { (* Update the position regarding newlines in [s] *)
      let start_p  = Lexing.lexeme_start_p lexbuf in
      let pos_bol  = ref start_p.pos_bol in
      let pos_lnum = ref start_p.pos_lnum in
      StringLabels.iteri s ~f:(fun i c ->
         match c with
         | '\n' -> pos_bol := start_p.pos_cnum + 1 + i; incr pos_lnum
         | _ -> ());
      lexbuf.lex_curr_p <-
        { lexbuf.lex_curr_p with
          pos_bol  = !pos_bol
        ; pos_lnum = !pos_lnum
        };
      let s = Scanf.unescaped s in
      new_sexp main (Sexp (Atom s, atom_loc lexbuf, stack)) lexbuf }
  | unquoted* as s
    { new_sexp main (Sexp (Atom s, atom_loc lexbuf, stack)) lexbuf }
  | eof
    { match stack with
      | Empty -> None
      | _     -> error lexbuf "unterminated s-expression" }
  | _
    { error lexbuf "syntax error" }

and trailing = parse
  | lf | dos_newline
    { Lexing.new_line lexbuf; trailing lexbuf }
  | blank+
    { trailing lexbuf }
  | (';' (_ # lf_cr)*)
    { trailing lexbuf }
  | eof
    { () }
  | _
    { error lexbuf "garbage after s-expression" }

{
  let single lexbuf =
    match main Empty lexbuf with
    | None -> error lexbuf "no s-expression found"
    | Some sexp -> trailing lexbuf; sexp

  let many lexbuf =
    let rec loop acc =
      match main Empty lexbuf with
      | None -> List.rev acc
      | Some sexp -> loop (sexp :: acc)
    in
    loop []
}
