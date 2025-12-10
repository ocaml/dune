type t =
  [ `Int of int
  | `Float of float
  | `String of string
  | `List of t list
  | `Bool of bool
  | `Assoc of (string * t) list
  | `Null
  ]

let copy_substring s buf start pos =
  if pos > start then Buffer.add_substring buf s start (pos - start)
;;

let rec quote_characters_to_buf s buf n start pos =
  (* check if a character is a valid utf-8 continuation byte *)
  let is_cb i = i < n && Char.code s.[i] land 0xc0 = 0x80 [@@inline] in
  if pos < n
  then (
    match s.[pos] with
    | '\b' -> escape s buf n start pos "\\b"
    | '\t' -> escape s buf n start pos "\\t"
    | '\n' -> escape s buf n start pos "\\n"
    | '\012' -> escape s buf n start pos "\\f"
    | '\r' -> escape s buf n start pos "\\r"
    | '\\' -> escape s buf n start pos "\\\\"
    | '"' -> escape s buf n start pos "\\\""
    | '\000' .. '\031' as c ->
      escape s buf n start pos (Printf.sprintf "\\u%04x" (Char.code c))
    | '\032' .. '\127' -> quote_characters_to_buf s buf n start (pos + 1)
    (* Check for valid UTF-8 *)
    | '\xc0' .. '\xdf' when is_cb (pos + 1) ->
      quote_characters_to_buf s buf n start (pos + 2)
    | '\xe0' .. '\xef' when is_cb (pos + 1) && is_cb (pos + 2) ->
      quote_characters_to_buf s buf n start (pos + 3)
    | '\xf0' .. '\xf7' when is_cb (pos + 1) && is_cb (pos + 2) && is_cb (pos + 3) ->
      quote_characters_to_buf s buf n start (pos + 4)
    (* Replace unrepresentable bytes by the Unicode replacement character (0xFFFD),
         encoded in UTF-8 *)
    | _ -> escape s buf n start pos "\xef\xbf\xbd")
  else copy_substring s buf start pos

and escape s buf n start pos e =
  copy_substring s buf start pos;
  Buffer.add_string buf e;
  quote_characters_to_buf s buf n (pos + 1) (pos + 1)
;;

let quote_string_to_buf s buf =
  Buffer.add_char buf '"';
  quote_characters_to_buf s buf (String.length s) 0 0;
  Buffer.add_char buf '"'
;;

let rec to_buf t buf =
  match t with
  | `String s -> quote_string_to_buf s buf
  | `Int i -> Buffer.add_string buf (string_of_int i)
  | `Float f -> Buffer.add_string buf (Printf.sprintf "%.17g" f)
  | `Bool b -> Buffer.add_string buf (string_of_bool b)
  | `List l ->
    Buffer.add_char buf '[';
    array_body_to_buf l buf;
    Buffer.add_char buf ']'
  | `Assoc o ->
    Buffer.add_char buf '{';
    object_body_to_buf o buf;
    Buffer.add_char buf '}'
  | `Null -> Buffer.add_string buf "null"

and array_body_to_buf t buf =
  match t with
  | [] -> ()
  | [ x ] -> to_buf x buf
  | x :: xs ->
    to_buf x buf;
    Buffer.add_char buf ',';
    array_body_to_buf xs buf

and object_body_to_buf t buf =
  match t with
  | [] -> ()
  | [ (x, y) ] ->
    quote_string_to_buf x buf;
    Buffer.add_char buf ':';
    to_buf y buf
  | (x, y) :: xs ->
    quote_string_to_buf x buf;
    Buffer.add_char buf ':';
    to_buf y buf;
    Buffer.add_char buf ',';
    object_body_to_buf xs buf
;;

let to_string t =
  let buf = Buffer.create 0 in
  to_buf t buf;
  Buffer.contents buf
;;

let string s : t = `String s
let assoc (xs : (string * t) list) : t = `Assoc xs
let list (xs : t list) : t = `List xs
let int (x : int) : t = `Int x
let float (x : float) : t = `Float x
