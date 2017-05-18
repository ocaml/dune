open Import

type t =
  { start : Lexing.position
  ; stop  : Lexing.position
  }

let of_lexbuf lb =
  { start = Lexing.lexeme_start_p lb
  ; stop  = Lexing.lexeme_end_p   lb
  }

exception Error of t * string

let fail t fmt =
  ksprintf (fun msg -> raise (Error (t, msg))) fmt

let fail_lex lb fmt =
  fail (of_lexbuf lb) fmt

let in_file fn =
  let pos : Lexing.position =
    { pos_fname = fn
    ; pos_lnum  = 1
    ; pos_cnum  = 0
    ; pos_bol   = 0
    }
  in
  { start = pos
  ; stop = pos
  }

let none = in_file "<none>"

let print ppf { start; stop } =
  let start_c = start.pos_cnum - start.pos_bol in
  let stop_c  = stop.pos_cnum  - start.pos_bol in
  Format.fprintf ppf
    "@{<loc>File \"%s\", line %d, characters %d-%d:@}@\n"
    start.pos_fname start.pos_lnum start_c stop_c

let warn t fmt =
  Format.eprintf ("%a@{<warning>Warning@}: " ^^ fmt ^^ "@.") print t
