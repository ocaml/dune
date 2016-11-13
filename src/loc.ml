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
  Printf.ksprintf (fun msg -> raise (Error (t, msg))) fmt

let fail_lex lb fmt =
  fail (of_lexbuf lb) fmt
