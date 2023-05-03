(**************************************************************************)
(*                                                                        *)
(*    Copyright 2012-2019 OCamlPro                                        *)
(*    Copyright 2012 INRIA                                                *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

(* Stubbed-out console file.
   Dune never wants OPAM to print anything, so every value in the interface
   is replaced by a no-op stub *)

let debug () = false

let verbose () = false

let color () = false

let disp_status_line () = false

let utf8, utf8_extended = (fun () -> false), (fun () -> false)

module Symbols = struct
  let rightwards_arrow = Uchar.min
  let box_drawings_light_down_and_right = Uchar.min
  let box_drawings_light_horizontal = Uchar.min
  let box_drawings_light_vertical = Uchar.min
  let box_drawings_light_up_and_right = Uchar.min
  let box_drawings_light_right = Uchar.min
  let circled_division_slash = Uchar.min
  let asterisk_operator = Uchar.min
  let north_east_arrow = Uchar.min
  let south_east_arrow = Uchar.min
  let clockwise_open_circle_arrow = Uchar.min
  let greek_small_letter_lambda = Uchar.min
  let latin_capital_letter_o_with_stroke = Uchar.min
  let six_pointed_black_star = Uchar.min
  let upwards_arrow = Uchar.min
  let downwards_arrow = Uchar.min
  let up_down_arrow = Uchar.min
  let downwards_double_arrow = Uchar.min
  let black_down_pointing_triangle = Uchar.min
  let downwards_black_arrow = Uchar.min
end

let utf8_symbol _main ?alternates:_ s = s

let timer () () = 0.

type text_style =
  [ `bold
  | `underline
  | `crossed
  | `black
  | `red
  | `green
  | `yellow
  | `blue
  | `magenta
  | `cyan
  | `white ]

(* not nestable *)
let colorise _style s = s

let colorise' _styles s = s

let acolor c () = colorise c
let acolor_w _width _c _f _s = ()



let carriage_delete () = ()

let clear_status () = ()

let log _section ?level:_ fmt =
  let null_formatter = Format.make_formatter (fun _ _ _ -> ()) (fun () -> ()) in
  Format.ifprintf null_formatter fmt

let slog _to_string _f _x = ()

let error fmt =
  Printf.ifprintf () fmt

let warning fmt =
  Printf.ifprintf () fmt

let note fmt =
  Printf.ifprintf () fmt

let errmsg fmt =
  Printf.ifprintf () fmt

let error_and_exit _reason fmt =
  Printf.ikfprintf (fun () -> failwith "never") () fmt

let msg fmt =
  Printf.ifprintf () fmt

let formatted_msg ?indent:_ fmt =
  Printf.ifprintf () fmt

let status_line fmt =
  Printf.ifprintf () fmt

let header_msg fmt =
  Printf.ifprintf () fmt

let header_error fmt =
  Printf.ikfprintf (fun () -> failwith "never") () fmt

let pause fmt =
  Printf.ifprintf () fmt

let confirm ?require_unsafe_yes:_ ?default:_ fmt =
  Printf.ikfprintf (fun () -> false) () fmt

let read fmt =
  Printf.ikfprintf (fun () -> None) () fmt

let print_table ?cut:_ _oc ~sep:_ _table = ()

let menu ?default:_ ?unsafe_yes:_ ?yes:_ ~no:_ ~options:_ fmt =
  Printf.ikfprintf (fun () -> failwith "never") () fmt

(** Tree printing *)
module Tree = struct

  type 'elt t = 'elt

  let value x = x
  let children _ = []

  let create ?children:_ value = value

  type symbols = {
    vert: string;
    hor:  string;
    tee:  string;
    hook: string;
  }

  let get_default_symbols () =
    { vert=""; hor=""; tee=""; hook="" }

  let print ?symbols:_ ~printer:_ _ = ()
end
