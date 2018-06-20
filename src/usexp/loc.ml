type t =
  { start : Lexing.position
  ; stop  : Lexing.position
  }

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
