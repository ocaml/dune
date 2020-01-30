(* Copyright (c) 2020 David Kaloper Meršinjak. All rights reserved.
   See LICENSE.md. *)

(* Unpacked interval lookup table. *)
let find_i ~def k (xs, _, _ as tab) =
  let rec go i j (los, his, vs as tab) (k: int) def =
    if i > j then def else
    let x = (i + j) / 2 in
    if k < Array.unsafe_get los x then go i (x - 1) tab k def else
    if k > Array.unsafe_get his x then go (x + 1) j tab k def else
      Array.unsafe_get vs x in
  go 0 (Array.length xs - 1) tab k def

(* 12-6-6-bit (0xfff-0x3f-0x3f) trie, 3 levels, array-array-string.
   Root is variable; lower levels are either empty or complete. *)
let find_t ~def k tab =
  let k = if k > 0xd7ff then k - 0x800 else k in (* Pack to continuous range. *)
  let b0 = (k lsr 12) land 0xfff in
  if Array.length tab <= b0 then def else
  match Array.unsafe_get tab b0 with
  | [||] -> def
  | arr -> match Array.unsafe_get arr ((k lsr 6) land 0x3f) with
    | "" -> def
    | str -> String.unsafe_get str (k land 0x3f) |> Char.code

(* We catch w = -1 and default to w = 1 to minimize the table. *)
let tty_width_hint u = match Uchar.to_int u with
| 0 -> 0
| u when u <= 0x001F || 0x007F <= u && u <= 0x009F -> -1
| u when u <= 0x02ff -> 1
| u -> find_i ~def:1 u Notty_uucp_data.tty_width_hint

let grapheme_cluster_boundary u =
  find_t ~def:16 (Uchar.to_int u) Notty_uucp_data.grapheme_cluster_boundary

(* let check () = *)
(*   let pp_u ppf u = Format.fprintf ppf "u+%04x" (Uchar.to_int u) in *)
(*   let rec go i u = *)
(*     let w1 = tty_width_hint u *)
(*     and w2 = Uucp.Break.tty_width_hint u in *)
(*     if w1 <> w2 then Format.printf "w: %a here: %d there: %d@." pp_u u w1 w2; *)
(*     let gc1 = grapheme_cluster_boundary u *)
(*     and gc2 = Uucp.Break.Low.grapheme_cluster u in *)
(*     if gc1 <> gc2 then Format.printf "gc: %a here: %d there: %d@." pp_u u gc1 gc2; *)
(*     if u = Uchar.max then i else go (i + 1) (Uchar.succ u) in *)
(*   let n = go 1 Uchar.min in *)
(*   Format.printf "Checked equality for %d code points.@." n *)

