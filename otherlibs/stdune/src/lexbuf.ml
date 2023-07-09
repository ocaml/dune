type t = Lexing.lexbuf

module Position = struct
  type t = Lexing.position

  let equal
      { Lexing.pos_fname = f_a; pos_lnum = l_a; pos_bol = b_a; pos_cnum = c_a }
      { Lexing.pos_fname = f_b; pos_lnum = l_b; pos_bol = b_b; pos_cnum = c_b }
      =
    f_a = f_b && l_a = l_b && b_a = b_b && c_a = c_b
end

module Loc = struct
  type t =
    { start : Lexing.position
    ; stop : Lexing.position
    }
end

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
