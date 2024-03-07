type t = Lexing.lexbuf

module Position = struct
  type t = Lexing.position

  let equal
    { Lexing.pos_fname = f_a; pos_lnum = l_a; pos_bol = b_a; pos_cnum = c_a }
    { Lexing.pos_fname = f_b; pos_lnum = l_b; pos_bol = b_b; pos_cnum = c_b }
    =
    f_a = f_b && l_a = l_b && b_a = b_b && c_a = c_b
  ;;

  let in_file ~fname =
    { Lexing.pos_fname = fname; pos_lnum = 1; pos_cnum = 0; pos_bol = 0 }
  ;;

  let none = in_file ~fname:"<none>"

  let to_dyn { Lexing.pos_fname; pos_lnum; pos_bol; pos_cnum } =
    let open Dyn in
    Record
      [ "pos_lnum", Int pos_lnum
      ; "pos_bol", Int pos_bol
      ; "pos_cnum", Int pos_cnum
      ; "pos_fname", String pos_fname
      ]
  ;;

  let to_dyn_no_file (p : t) =
    let open Dyn in
    Record
      [ "pos_lnum", Int p.pos_lnum; "pos_bol", Int p.pos_bol; "pos_cnum", Int p.pos_cnum ]
  ;;

  let is_file_only { Lexing.pos_fname = _; pos_lnum; pos_cnum; pos_bol } =
    pos_lnum = none.pos_lnum && pos_cnum = none.pos_cnum && pos_bol = none.pos_bol
  ;;
end

module Loc = struct
  type t =
    { start : Lexing.position
    ; stop : Lexing.position
    }

  let of_pos (pos_fname, pos_lnum, cnum, enum) =
    let start : Lexing.position = { pos_fname; pos_lnum; pos_cnum = cnum; pos_bol = 0 } in
    { start; stop = { start with pos_cnum = enum } }
  ;;

  let map_pos { start; stop } ~f = { start = f start; stop = f stop }

  let in_file ~fname =
    let start = Position.in_file ~fname in
    { start; stop = start }
  ;;

  let is_file_only t =
    t.start.pos_fname = t.stop.pos_fname
    && Position.is_file_only t.start
    && Position.is_file_only t.stop
  ;;

  let to_dyn t =
    let open Dyn in
    Record
      [ "pos_fname", String t.start.pos_fname
      ; "start", Position.to_dyn_no_file t.start
      ; "stop", Position.to_dyn_no_file t.stop
      ]
  ;;

  let compare = Poly.compare
  let equal x y = Ordering.is_eq (compare x y)
  let none = { start = Position.none; stop = Position.none }
end

let init (t : t) ~fname =
  t.lex_curr_p <- { pos_fname = fname; pos_lnum = 1; pos_bol = 0; pos_cnum = 0 }
;;

let from_string s ~fname =
  let t = Lexing.from_string s in
  init t ~fname;
  t
;;

let from_channel ic ~fname =
  let t = Lexing.from_channel ic in
  init t ~fname;
  t
;;
