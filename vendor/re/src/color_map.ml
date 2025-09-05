(* In reality, this can really be represented as a bool array.

   The representation is best thought of as a list of all chars along with a
   flag:

   (a, 0), (b, 1), (c, 0), (d, 0), ...

   characters belonging to the same color are represented by sequnces of
   characters with the flag set to 0.
*)

type t = Bytes.t

module Repr = struct
  type t = string

  let repr t color = t.[Cset.to_int color]
  let length = String.length
end

module Table = struct
  type t = string

  let get_char t c = t.[Cset.to_int c]
  let get t c = Cset.of_char (String.unsafe_get t (Char.code c))

  let translate_colors (cm : t) cset =
    Cset.fold_right cset ~init:Cset.empty ~f:(fun i j l ->
      let start = get_char cm i in
      let stop = get_char cm j in
      Cset.union (Cset.cseq start stop) l)
  ;;
end

let make () = Bytes.make 257 '\000'

let flatten cm =
  let c = Bytes.create 256 in
  let color_repr = Bytes.create 256 in
  let v = ref 0 in
  Bytes.set c 0 '\000';
  Bytes.set color_repr 0 '\000';
  for i = 1 to 255 do
    if Bytes.get cm i <> '\000' then incr v;
    Bytes.set c i (Char.chr !v);
    Bytes.set color_repr !v (Char.chr i)
  done;
  Bytes.unsafe_to_string c, Bytes.sub_string color_repr 0 (!v + 1)
;;

(* mark all the endpoints of the intervals of the char set with the 1 byte *)
let split t set =
  Cset.iter set ~f:(fun i j ->
    Bytes.set t (Cset.to_int i) '\001';
    Bytes.set t (Cset.to_int j + 1) '\001')
;;
