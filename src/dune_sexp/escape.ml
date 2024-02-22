open! Stdune

module Utf8 = struct
  (*
     The first byte of an utf8 character gives the size in bytes of the utf8:

     0xxxxxxxx -> 1
     110xxxxxx -> 2
     1110xxxxx -> 3
     11110xxxx -> 4
  *)
  let utf8_byte_length u =
    match Char.code u with
    | u when u < 128 -> 1
    | u when u < 194 -> 0
    | u when u < 224 -> 2
    | u when u < 240 -> 3
    | u when u < 245 -> 4
    | _ -> 0
  ;;

  let unsafe_get s i = String.unsafe_get s i |> Char.code
  let next_utf8_length s i = utf8_byte_length (String.unsafe_get s i)

  let is_utf8_valid s i l =
    assert (String.length s >= l);
    match l with
    | 1 -> true
    | 2 ->
      let b1 = unsafe_get s (i + 1) in
      if b1 lsr 6 != 0b10 then false else true
    | 3 ->
      let b0 = unsafe_get s i in
      let b1 = unsafe_get s (i + 1) in
      let b2 = unsafe_get s (i + 2) in
      if b2 lsr 6 != 0b10
      then false
      else (
        match b0 with
        | 0xE0 -> if b1 < 0xA0 || 0xBF < b1 then false else true
        | 0xED -> if b1 < 0x80 || 0x9F < b1 then false else true
        | _ -> if b1 lsr 6 != 0b10 then false else true)
    | 4 ->
      let b0 = unsafe_get s i in
      let b1 = unsafe_get s (i + 1) in
      let b2 = unsafe_get s (i + 2) in
      let b3 = unsafe_get s (i + 3) in
      if b3 lsr 6 != 0b10 || b2 lsr 6 != 0b10
      then false
      else (
        match b0 with
        | 0xF0 -> if b1 < 0x90 || 0xBF < b1 then false else true
        | 0xF4 -> if b1 < 0x80 || 0x8F < b1 then false else true
        | _ -> if b1 lsr 6 != 0b10 then false else true)
    | _ -> false
  ;;
end

let quote_length s =
  let n = ref 0 in
  let len = String.length s in
  let i = ref 0 in
  while !i < len do
    (n
     := !n
        +
        match String.unsafe_get s !i with
        | '\"' | '\\' | '\n' | '\t' | '\r' | '\b' -> 2
        | '%' -> if !i + 1 < len && s.[!i + 1] = '{' then 2 else 1
        | ' ' .. '~' -> 1
        | _ ->
          let uchar_len = Utf8.next_utf8_length s !i in
          (match Utf8.is_utf8_valid s !i uchar_len with
           | true ->
             assert (uchar_len > 1 && uchar_len < 5);
             i := !i + uchar_len - 1;
             uchar_len
           | false -> 4));
    incr i
  done;
  !n
;;

let escape_to s ~dst:s' ~ofs =
  let n = ref ofs in
  let len = String.length s in
  let i = ref 0 in
  while !i < len do
    (match String.unsafe_get s !i with
     | ('\"' | '\\') as c ->
       Bytes.unsafe_set s' !n '\\';
       incr n;
       Bytes.unsafe_set s' !n c
     | '\n' ->
       Bytes.unsafe_set s' !n '\\';
       incr n;
       Bytes.unsafe_set s' !n 'n'
     | '\t' ->
       Bytes.unsafe_set s' !n '\\';
       incr n;
       Bytes.unsafe_set s' !n 't'
     | '\r' ->
       Bytes.unsafe_set s' !n '\\';
       incr n;
       Bytes.unsafe_set s' !n 'r'
     | '\b' ->
       Bytes.unsafe_set s' !n '\\';
       incr n;
       Bytes.unsafe_set s' !n 'b'
     | '%' when !i + 1 < len && s.[!i + 1] = '{' ->
       Bytes.unsafe_set s' !n '\\';
       incr n;
       Bytes.unsafe_set s' !n '%'
     | ' ' .. '~' as c -> Bytes.unsafe_set s' !n c
     | c ->
       let uchar_len = Utf8.next_utf8_length s !i in
       (match Utf8.is_utf8_valid s !i uchar_len with
        | true ->
          assert (uchar_len > 1 && uchar_len < 5);
          Bytes.unsafe_set s' !n (String.unsafe_get s !i);
          Bytes.unsafe_set s' (!n + 1) (String.unsafe_get s (!i + 1));
          if uchar_len > 2
          then Bytes.unsafe_set s' (!n + 2) (String.unsafe_get s (!i + 2));
          if uchar_len > 3
          then Bytes.unsafe_set s' (!n + 3) (String.unsafe_get s (!i + 3));
          n := !n + uchar_len - 1;
          i := !i + uchar_len - 1
        | false ->
          let a = Char.code c in
          Bytes.unsafe_set s' !n '\\';
          incr n;
          Bytes.unsafe_set s' !n (Char.unsafe_chr (48 + (a / 100)));
          incr n;
          Bytes.unsafe_set s' !n (Char.unsafe_chr (48 + (a / 10 mod 10)));
          incr n;
          Bytes.unsafe_set s' !n (Char.unsafe_chr (48 + (a mod 10)))));
    incr n;
    incr i
  done
;;

(* Escape [s] if needed. *)
let escaped s =
  let n = quote_length s in
  if n = 0 || n > String.length s
  then (
    let s' = Bytes.create n in
    escape_to s ~dst:s' ~ofs:0;
    Bytes.unsafe_to_string s')
  else s
;;

(* Surround [s] with quotes, escaping it if necessary. *)
let quoted s =
  let len = String.length s in
  let n = quote_length s in
  let s' = Bytes.create (n + 2) in
  Bytes.unsafe_set s' 0 '"';
  if len = 0 || n > len
  then escape_to s ~dst:s' ~ofs:1
  else Bytes.blit_string ~src:s ~src_pos:0 ~dst:s' ~dst_pos:1 ~len;
  Bytes.unsafe_set s' (n + 1) '"';
  Bytes.unsafe_to_string s'
;;
