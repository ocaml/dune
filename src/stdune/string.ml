(* Because other the syntax s.[x] causes trouble *)
module String = Caml.String

include struct
  [@@@warning "-32-3"]
  let capitalize_ascii   = String.capitalize
  let uncapitalize_ascii = String.uncapitalize
  let uppercase_ascii    = String.uppercase
  let lowercase_ascii    = String.lowercase
end

include StringLabels

let compare a b = Ordering.of_int (String.compare a b)

let capitalize   = capitalize_ascii
let uncapitalize = uncapitalize_ascii
let uppercase    = uppercase_ascii
let lowercase    = lowercase_ascii

let break s ~pos =
  (sub s ~pos:0 ~len:pos,
   sub s ~pos ~len:(length s - pos))

let is_prefix s ~prefix =
  let len = length s in
  let prefix_len = length prefix in
  len >= prefix_len &&
  sub s ~pos:0 ~len:prefix_len = prefix

let is_suffix s ~suffix =
  let len = length s in
  let suffix_len = length suffix in
  len >= suffix_len &&
  sub s ~pos:(len - suffix_len) ~len:suffix_len = suffix

let drop_prefix s ~prefix =
  if is_prefix s ~prefix then
    if length s = length prefix then
      Some ""
    else
      Some (sub s ~pos:(length prefix) ~len:(length s - length prefix))
  else
    None

let extract_words s ~is_word_char =
  let rec skip_blanks i =
    if i = length s then
      []
    else if is_word_char s.[i] then
      parse_word i (i + 1)
    else
      skip_blanks (i + 1)
  and parse_word i j =
    if j = length s then
      [sub s ~pos:i ~len:(j - i)]
    else if is_word_char s.[j] then
      parse_word i (j + 1)
    else
      sub s ~pos:i ~len:(j - i) :: skip_blanks (j + 1)
  in
  skip_blanks 0

let extract_comma_space_separated_words s =
  extract_words s ~is_word_char:(function
    | ',' | ' ' | '\t' | '\n' -> false
    | _ -> true)

let extract_blank_separated_words s =
  extract_words s ~is_word_char:(function
    | ' ' | '\t' -> false
    | _ -> true)

let lsplit2 s ~on =
  match index s on with
  | exception Not_found -> None
  | i ->
    Some
      (sub s ~pos:0 ~len:i,
       sub s ~pos:(i + 1) ~len:(length s - i - 1))

let rsplit2 s ~on =
  match rindex s on with
  | exception Not_found -> None
  | i ->
    Some
      (sub s ~pos:0 ~len:i,
       sub s ~pos:(i + 1) ~len:(length s - i - 1))

let index s ch =
  match index s ch with
  | i -> Some i
  | exception Not_found -> None

let split s ~on =
  let rec loop i j =
    if j = length s then
      [sub s ~pos:i ~len:(j - i)]
    else if s.[j] = on then
      sub s ~pos:i ~len:(j - i) :: loop (j + 1) (j + 1)
    else
      loop i (j + 1)
  in
  loop 0 0

let split_lines s =
  let rec loop ~last_is_cr ~acc i j =
    if j = length s then (
      let acc =
        if j = i || (j = i + 1 && last_is_cr) then
          acc
        else
          sub s ~pos:i ~len:(j - i) :: acc
      in
      List.rev acc
    ) else
      match s.[j] with
      | '\r' -> loop ~last_is_cr:true ~acc i (j + 1)
      | '\n' ->
        let line =
          let len = if last_is_cr then j - i - 1 else j - i in
          sub s ~pos:i ~len
        in
        loop ~acc:(line :: acc) (j + 1) (j + 1) ~last_is_cr:false
      | _ ->
        loop ~acc i (j + 1) ~last_is_cr:false
  in
  loop ~acc:[] 0 0 ~last_is_cr:false

let escape_double_quote s =
  let n = ref 0 in
  let len = length s in
  for i = 0 to len - 1 do
    if unsafe_get s i = '"' then incr n;
  done;
  if !n = 0 then s
  else (
    let b = Bytes.create (len + !n) in
    n := 0;
    for i = 0 to len - 1 do
      if unsafe_get s i = '"' then (
        Bytes.unsafe_set b !n '\\';
        incr n;
      );
      Bytes.unsafe_set b !n (unsafe_get s i);
      incr n
    done;
    Bytes.unsafe_to_string b
  )

let longest_map l ~f =
  List.fold_left l ~init:0 ~f:(fun acc x ->
    max acc (length (f x)))

let longest l = longest_map l ~f:(fun x -> x)
