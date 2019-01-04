type t =
  { start : Lexing.position
  ; stop  : Lexing.position
  }

let print ppf { start; stop } =
  let start_c = start.pos_cnum - start.pos_bol in
  let stop_c  = stop.pos_cnum  - start.pos_bol in
  Format.fprintf ppf
    "@{<loc>File \"%s\", line %d, characters %d-%d:@}@\n"
    start.pos_fname start.pos_lnum start_c stop_c
