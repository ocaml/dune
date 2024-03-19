open! Stdune

(** Note: on OCaml >= 4.14, this can be switched to the following (and the
    dependency to [Uutf] can be removed)

    {[
      let next_valid_utf8_length s i =
        let decode = String.get_utf_8_uchar s i in
        Option.some_if (Uchar.utf_decode_is_valid decode) (Uchar.utf_decode_length decode)
      ;;
    ]} *)
let next_valid_utf8_uchar_len s i =
  let pos = ref i in
  let buf = Bytes.create 1 in
  let decoder = Uutf.decoder ~encoding:`UTF_8 `Manual in
  let rec go () =
    match Uutf.decode decoder with
    | `Await ->
      if !pos >= String.length s
      then None
      else (
        Bytes.set buf 0 (String.get s !pos);
        incr pos;
        Uutf.Manual.src decoder buf 0 1;
        go ())
    | `Uchar _ -> Some (!pos - i)
    | `Malformed _ -> None
    | `End -> Code_error.raise "next_valid_utf8_uchar: `End" []
  in
  go ()
;;

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
          (match next_valid_utf8_uchar_len s !i with
           | Some uchar_len ->
             i := !i + uchar_len - 1;
             uchar_len
           | None -> 4));
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
       (match next_valid_utf8_uchar_len s !i with
        | Some uchar_len ->
          Bytes.unsafe_set s' !n (String.unsafe_get s !i);
          Bytes.unsafe_set s' (!n + 1) (String.unsafe_get s (!i + 1));
          if uchar_len > 2
          then Bytes.unsafe_set s' (!n + 2) (String.unsafe_get s (!i + 2));
          if uchar_len > 3
          then Bytes.unsafe_set s' (!n + 3) (String.unsafe_get s (!i + 3));
          n := !n + uchar_len - 1;
          i := !i + uchar_len - 1
        | None ->
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
