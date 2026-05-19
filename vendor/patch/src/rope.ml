(* Originally from https://github.com/robur-coop/utcp, written by
   Calascibetta Romain <romain.calascibetta@gmail.com> *)

(* A rope data structure where each node is a line *)

type t =
  | Str of string array * bool * int * int
  | App of t * t * int

let length = function
  | Str (_, _, len, _) -> len
  | App (_, _, len) -> len

(* keep compatibility with 4.08 *)
let min_int (a : int) (b : int) = min a b
external unsafe_blit_string : string -> int -> bytes -> int -> int -> unit
  = "caml_blit_string" [@@noalloc]

let append t1 t2 =
  App (t1, t2, length t1 + length t2)

let empty = Str (Array.make 0 "", true, 0, 0)

let rec unsafe_sub t start stop =
  if start = 0 && Int.equal stop (length t) then
    t
  else if Int.equal start stop then
    empty
  else match t with
    | Str (data, nl, len, off) ->
      assert (stop <= (len : int));
      Str (data, nl, stop - start, off + start)
    | App (l, r, _) ->
        let len = length l in
        if stop <= (len : int) then unsafe_sub l start stop
        else if start >= (len : int) then unsafe_sub r (start - len) (stop - len)
        else append (unsafe_sub l start len) (unsafe_sub r 0 (stop - len))

let chop t ?(off = 0) len =
  if len < 0 || len > (length t - off : int) then
    invalid_arg "Rope.chop";
  if len = 0 then empty else unsafe_sub t off (off + len)

let shift t len =
  if len < 0 then
    invalid_arg "Rope.shift";
  if len = 0 then
    t
  else
    let max = length t in
    let len = min_int max len in
    let l = len + (max - len) in
    unsafe_sub t len l

let rec last_is_nl = function
  | Str (a, nl, len, off) -> if Int.equal (Array.length a - off) len then nl else true
  | App (_, r, _) -> last_is_nl r

let rec byte_length = function
  | Str (s, _, len, off) as a ->
    let sum = ref 0 in
    for idx = off to len + off - 1 do
      let data = Array.unsafe_get s idx in
      sum := !sum + String.length data + 1
    done;
    !sum - if last_is_nl a then 0 else 1
  | App (l, r, _) -> byte_length l + byte_length r

let rec into_bytes buf dst_off = function
  | Str (s, _, len, off) as a ->
    let off' = ref dst_off in
    for idx = off to len + off - 1 do
      let data = Array.unsafe_get s idx in
      unsafe_blit_string data 0 buf !off' (String.length data);
      off' := !off' + String.length data + 1;
      if idx - off < (len - 1 : int) || (Int.equal (idx - off) (len - 1) && last_is_nl a) then
        Bytes.unsafe_set buf (!off' - 1) '\n'
    done
  | App (l, r, _) ->
    into_bytes buf dst_off l;
    into_bytes buf (dst_off + byte_length l) r

let to_string t =
  let len = byte_length t in
  let buf = Bytes.create len in
  into_bytes buf 0 t;
  Bytes.unsafe_to_string buf

let concat a b = append a b

let of_strings xs last_is_nl =
  let d = Array.of_list xs in
  Str (d, last_is_nl, Array.length d, 0)

let of_string str =
  let splitted = String.split_on_char '\n' str in
  let last_is_nl = String.unsafe_get str (String.length str - 1) = '\n' in
  let d = Array.of_list splitted in
  Str (d, last_is_nl, Array.length d - (if last_is_nl then 1 else 0), 0)

let rec equal_to_string_list t = function
  | [] -> length t = 0
  | hd :: tl ->
    let rec find_data = function
      | Str (data, _, len, off) ->
        if len > 0 then Some (Array.get data off) else None
      | App (l, r, _) ->
        if length l > 0 then
          find_data l
        else
          find_data r
    in
    match find_data t with
    | None -> false
    | Some data ->
      String.equal hd data &&
      equal_to_string_list (shift t 1) tl
