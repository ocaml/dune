type t =
  { start : Lexing.position
  ; stop  : Lexing.position
  }

let none_pos p : Lexing.position =
  { pos_fname = p
  ; pos_lnum  = 1
  ; pos_cnum  = 0
  ; pos_bol   = 0
  }

let none =
  let pos = none_pos "<none>" in
  { start = pos
  ; stop = pos
  }
