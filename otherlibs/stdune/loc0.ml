type t =
  { start : Lexing.position
  ; stop : Lexing.position
  }

let none_pos p : Lexing.position =
  { pos_fname = p; pos_lnum = 1; pos_cnum = 0; pos_bol = 0 }

let none =
  let pos = none_pos "<none>" in
  { start = pos; stop = pos }

let dyn_of_position_no_file (p : Lexing.position) =
  let open Dyn in
  Record
    [ ("pos_lnum", Int p.pos_lnum)
    ; ("pos_bol", Int p.pos_bol)
    ; ("pos_cnum", Int p.pos_cnum)
    ]

let to_dyn t =
  let open Dyn in
  Record
    [ ("pos_fname", String t.start.pos_fname)
    ; ("start", dyn_of_position_no_file t.start)
    ; ("stop", dyn_of_position_no_file t.stop)
    ]
