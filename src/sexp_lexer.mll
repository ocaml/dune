{
type stack =
  | Empty
  | Open of Lexing.position * stack
  | Sexp of Sexp.t * Sexp.Locs.t * stack

let error = Loc.fail_lex

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

let char_for_backslash = function
  | 'n' -> '\010'
  | 'r' -> '\013'
  | 'b' -> '\008'
  | 't' -> '\009'
  | c -> c

let dec_code c1 c2 c3 =
  100 * (Char.code c1 - 48) + 10 * (Char.code c2 - 48) + (Char.code c3 - 48)

let hex_code c1 c2 =
  let d1 = Char.code c1 in
  let val1 =
    if d1 >= 97 then d1 - 87
    else if d1 >= 65 then d1 - 55
    else d1 - 48 in
  let d2 = Char.code c2 in
  let val2 =
    if d2 >= 97 then d2 - 87
    else if d2 >= 65 then d2 - 55
    else d2 - 48 in
  val1 * 16 + val2

let escaped_buf = Buffer.create 256
}

let lf = '\010'
let lf_cr = ['\010' '\013']
let dos_newline = "\013\010"
let blank = [' ' '\009' '\012']
let unquoted = [^ ';' '(' ')' '"'] # blank # lf_cr
let digit = ['0'-'9']
let hexdigit = ['0'-'9' 'a'-'f' 'A'-'F']

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
  | '"'
    { Buffer.clear escaped_buf;
      scan_string escaped_buf (Lexing.lexeme_start_p lexbuf) stack lexbuf
    }
  | "#|"
    { block_comment 0 stack lexbuf }
  | unquoted* as s
    { new_sexp main (Sexp (Atom s, atom_loc lexbuf, stack)) lexbuf }
  | eof
    { match stack with
      | Empty -> None
      | _     -> error lexbuf "unterminated s-expression" }
  | _
    { error lexbuf "syntax error" }

and block_comment depth stack = parse
  | "#|"
    { block_comment (depth + 1) stack lexbuf }
  | "|#"
    { if depth = 0 then
        main stack lexbuf
      else
        block_comment (depth - 1) stack lexbuf }
  | _
    { block_comment depth stack lexbuf }
  | eof
    { error lexbuf "unterminated block comment" }

and scan_string buf start stack = parse
  | '"'
      { new_sexp main
          (Sexp (Atom (Buffer.contents buf),
                 Atom { start; stop = Lexing.lexeme_end_p lexbuf },
                 stack))
          lexbuf
      }
  | '\\' lf
    {
      Lexing.new_line lexbuf;
      scan_string_after_escaped_newline buf start stack lexbuf
    }
  | '\\' dos_newline
    {
      Lexing.new_line lexbuf;
      scan_string_after_escaped_newline buf start stack lexbuf
    }
  | '\\' (['\\' '\'' '"' 'n' 't' 'b' 'r' ' '] as c)
      {
        Buffer.add_char buf (char_for_backslash c);
        scan_string buf start stack lexbuf
      }
  | '\\' (digit as c1) (digit as c2) (digit as c3)
      {
        let v = dec_code c1 c2 c3 in
        if v > 255 then error lexbuf "illegal escape";
        Buffer.add_char buf (Char.chr v);
        scan_string buf start stack lexbuf
      }
  | '\\' 'x' (hexdigit as c1) (hexdigit as c2)
      {
        let v = hex_code c1 c2 in
        Buffer.add_char buf (Char.chr v);
        scan_string buf start stack lexbuf
      }
  | '\\' (_ as c)
      {
        Buffer.add_char buf '\\';
        Buffer.add_char buf c;
        scan_string buf start stack lexbuf
      }
  | lf
      {
        Lexing.new_line lexbuf;
        Buffer.add_char buf '\n';
        scan_string buf start stack lexbuf
      }
  | ([^ '\\' '"'] # lf)+ as s
      {
        Buffer.add_string buf s;
        scan_string buf start stack lexbuf
      }
  | eof
      {
        error lexbuf "unterminated string"
      }

and scan_string_after_escaped_newline buf start stack = parse
  | [' ' '\t']*
    { scan_string buf start stack lexbuf }
  | ""
    { scan_string buf start stack lexbuf }

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
