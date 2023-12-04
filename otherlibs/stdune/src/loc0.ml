module Position = Lexbuf.Position

type t =
  | No_loc
  | In_file of string
  | Lexbuf_loc of Lexbuf.Loc.t
  | Same_file of
      { pos_fname : string
      ; start : Compact_position.t
      ; stop : Compact_position.t
      }
  | Same_line of
      { pos_fname : string
      ; loc : Compact_position.Same_line_loc.t
      }

open Lexbuf.Loc

let to_lexbuf_loc = function
  | No_loc -> Lexbuf.Loc.none
  | Lexbuf_loc loc -> loc
  | In_file fname -> Lexbuf.Loc.in_file ~fname
  | Same_file { pos_fname; start; stop } ->
    let start = Compact_position.to_position start ~fname:pos_fname in
    let stop = Compact_position.to_position stop ~fname:pos_fname in
    { Lexbuf.Loc.start; stop }
  | Same_line { pos_fname; loc } ->
    Compact_position.Same_line_loc.to_loc loc ~fname:pos_fname
;;

let in_file ~fname = In_file fname

let of_lexbuf_loc loc =
  if Lexbuf.Loc.(equal none loc)
  then No_loc
  else if Lexbuf.Loc.is_file_only loc
  then In_file loc.start.pos_fname
  else (
    let pos_fname = loc.start.pos_fname in
    match Compact_position.of_loc loc with
    | Same_line loc -> Same_line { pos_fname; loc }
    | Loc { start; stop } -> Same_file { pos_fname; start; stop }
    | Loc_does_not_fit -> Lexbuf_loc loc)
;;

let start = function
  | No_loc -> Lexbuf.Loc.none.start
  | Lexbuf_loc loc -> loc.start
  | In_file fname -> Position.in_file ~fname
  | Same_file { pos_fname; start; stop = _ } ->
    Compact_position.to_position start ~fname:pos_fname
  | Same_line { pos_fname; loc } ->
    Compact_position.Same_line_loc.start loc ~fname:pos_fname
;;

let stop = function
  | No_loc -> Lexbuf.Loc.none.stop
  | Lexbuf_loc loc -> loc.stop
  | In_file fname -> Position.in_file ~fname
  | Same_file { pos_fname; stop; start = _ } ->
    Compact_position.to_position stop ~fname:pos_fname
  | Same_line { pos_fname; loc } ->
    Compact_position.Same_line_loc.stop loc ~fname:pos_fname
;;

let compare = Poly.compare
let equal = Poly.equal
let none = No_loc

let is_none = function
  | No_loc -> true
  | _ -> false
;;

let to_dyn t = Lexbuf.Loc.to_dyn (to_lexbuf_loc t)
let set_stop t stop = of_lexbuf_loc { (to_lexbuf_loc t) with stop }
let set_start t start = of_lexbuf_loc { (to_lexbuf_loc t) with start }
let create ~start ~stop = of_lexbuf_loc { start; stop }
let map_pos t ~f = to_lexbuf_loc t |> Lexbuf.Loc.map_pos ~f |> of_lexbuf_loc

let set_start_to_stop = function
  | (No_loc | In_file _) as t -> t
  | Lexbuf_loc loc -> of_lexbuf_loc { loc with start = loc.stop }
  | Same_file t -> Same_file { t with start = t.stop }
  | Same_line t ->
    let loc = Compact_position.Same_line_loc.set_start_to_stop t.loc in
    Same_line { t with loc }
;;

let start_pos_cnum = function
  | No_loc | In_file _ -> Lexbuf.Loc.none.start.pos_cnum
  | Lexbuf_loc loc -> loc.start.pos_cnum
  | Same_file t -> Compact_position.cnum t.start
  | Same_line t -> Compact_position.Same_line_loc.start_cnum t.loc
;;

let stop_pos_cnum = function
  | No_loc | In_file _ -> Lexbuf.Loc.none.stop.pos_cnum
  | Lexbuf_loc loc -> loc.stop.pos_cnum
  | Same_file t -> Compact_position.cnum t.stop
  | Same_line t -> Compact_position.Same_line_loc.stop_cnum t.loc
;;
