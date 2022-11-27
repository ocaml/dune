open Stdune
open Import

type t = A of string [@@unboxed]

let to_dyn (A s) =
  let open Dyn in
  variant "A" [ string s ]

let equal (A a) (A b) = String.equal a b

let is_valid =
  let rec loop s i len =
    i = len
    ||
    match String.unsafe_get s i with
    | '%' -> after_percent s (i + 1) len
    | '"' | '(' | ')' | ';' | '\000' .. '\032' | '\127' .. '\255' -> false
    | _ -> loop s (i + 1) len
  and after_percent s i len =
    i = len
    ||
    match String.unsafe_get s i with
    | '%' -> after_percent s (i + 1) len
    | '"' | '(' | ')' | ';' | '\000' .. '\032' | '\127' .. '\255' | '{' -> false
    | _ -> loop s (i + 1) len
  in
  fun s ->
    let len = String.length s in
    len > 0 && loop s 0 len

let of_string s =
  if is_valid s then A s
  else
    Code_error.raise "Dune_lang.Atom.of_string got invalid atom"
      [ ("atom", String s) ]

let to_string (A s) = s

let parse s = if is_valid s then Some (A s) else None

let of_int i = A (string_of_int i)

let of_float x = of_string (string_of_float x)

let of_bool x = A (string_of_bool x)

let of_digest d = A (Digest.to_string d)

let of_int64 i = A (Int64.to_string i)
