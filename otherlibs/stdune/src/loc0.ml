type t =
  | No_loc
  | In_file of string
  | Lexbuf_loc of Lexbuf.Loc.t
  | Same_file of
      { pos_fname : string
      ; start_lnum : int
      ; start_bol : int
      ; start_cnum : int
      ; stop_lnum : int
      ; stop_bol : int
      ; stop_cnum : int
      }
  | Same_line of
      { pos_fname : string
      ; pos_lnum : int
      ; pos_bol : int
      ; start_cnum : int
      ; stop_cnum : int
      }

module Position = Lexbuf.Position
open Lexbuf.Loc

let to_lexbuf_loc = function
  | No_loc -> Lexbuf.Loc.none
  | Lexbuf_loc loc -> loc
  | In_file fname -> Lexbuf.Loc.in_file ~fname
  | Same_file
      { pos_fname
      ; start_lnum
      ; start_bol
      ; start_cnum
      ; stop_lnum
      ; stop_bol
      ; stop_cnum
      } ->
    let start =
      { Lexing.pos_fname
      ; pos_lnum = start_lnum
      ; pos_bol = start_bol
      ; pos_cnum = start_cnum
      }
    in
    let stop =
      { Lexing.pos_fname
      ; pos_lnum = stop_lnum
      ; pos_bol = stop_bol
      ; pos_cnum = stop_cnum
      }
    in
    { Lexbuf.Loc.start; stop }
  | Same_line { pos_fname; pos_lnum; pos_bol; start_cnum; stop_cnum } ->
    let start =
      { Lexing.pos_fname; pos_lnum; pos_bol; pos_cnum = start_cnum }
    in
    let stop = { Lexing.pos_fname; pos_lnum; pos_bol; pos_cnum = stop_cnum } in
    { Lexbuf.Loc.start; stop }

let in_file ~fname = In_file fname

let of_lexbuf_loc loc =
  if Lexbuf.Loc.(equal none loc) then No_loc
  else if Lexbuf.Loc.is_file_only loc then In_file loc.start.pos_fname
  else
    let start = loc.start in
    let stop = loc.stop in
    if start.pos_fname = stop.pos_fname then
      if start.pos_bol = stop.pos_bol && start.pos_lnum = stop.pos_lnum then
        Same_line
          { pos_fname = start.pos_fname
          ; pos_bol = start.pos_bol
          ; pos_lnum = start.pos_lnum
          ; start_cnum = start.pos_cnum
          ; stop_cnum = stop.pos_cnum
          }
      else
        Same_file
          { pos_fname = start.pos_fname
          ; start_lnum = start.pos_lnum
          ; start_bol = start.pos_bol
          ; start_cnum = start.pos_cnum
          ; stop_lnum = stop.pos_lnum
          ; stop_bol = stop.pos_bol
          ; stop_cnum = stop.pos_cnum
          }
    else Lexbuf_loc loc

let start = function
  | No_loc -> Lexbuf.Loc.none.start
  | Lexbuf_loc loc -> loc.start
  | In_file fname -> Position.in_file ~fname
  | Same_file { pos_fname; start_lnum; start_bol; start_cnum; _ } ->
    { Lexing.pos_fname
    ; pos_lnum = start_lnum
    ; pos_bol = start_bol
    ; pos_cnum = start_cnum
    }
  | Same_line { pos_fname; pos_lnum; pos_bol; start_cnum; _ } ->
    { Lexing.pos_fname; pos_lnum; pos_bol; pos_cnum = start_cnum }

let stop = function
  | No_loc -> Lexbuf.Loc.none.stop
  | Lexbuf_loc loc -> loc.stop
  | In_file fname -> Position.in_file ~fname
  | Same_file { pos_fname; stop_lnum; stop_bol; stop_cnum; _ } ->
    { Lexing.pos_fname
    ; pos_lnum = stop_lnum
    ; pos_bol = stop_bol
    ; pos_cnum = stop_cnum
    }
  | Same_line { pos_fname; pos_lnum; pos_bol; stop_cnum; _ } ->
    { Lexing.pos_fname; pos_lnum; pos_bol; pos_cnum = stop_cnum }

let compare = Poly.compare

let equal = Poly.equal

let none = No_loc

let is_none = function
  | No_loc -> true
  | _ -> false

let to_dyn t = Lexbuf.Loc.to_dyn (to_lexbuf_loc t)

let set_stop t stop = of_lexbuf_loc { (to_lexbuf_loc t) with stop }

let set_start t start = of_lexbuf_loc { (to_lexbuf_loc t) with start }

let create ~start ~stop = of_lexbuf_loc { start; stop }

let map_pos t ~f = to_lexbuf_loc t |> Lexbuf.Loc.map_pos ~f |> of_lexbuf_loc
