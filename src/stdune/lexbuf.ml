type t = Lexing.lexbuf

let init (t : t) ~fname =
  t.lex_curr_p <- { pos_fname = fname; pos_lnum = 1; pos_bol = 0; pos_cnum = 0 }

let from_string s ~fname =
  let t = Lexing.from_string s in
  init t ~fname;
  t

let from_channel ic ~fname =
  let t = Lexing.from_channel ic in
  init t ~fname;
  t
