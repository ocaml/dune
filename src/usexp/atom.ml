type t = A of string [@@unboxed]

let invalid_argf fmt = Printf.ksprintf invalid_arg fmt

type syntax = Jbuild | Dune

let (is_valid_jbuild, is_valid_dune) =
  let rec jbuild s i len =
    i = len ||
    match String.unsafe_get s i with
    | '#' -> disallow_next '|' s (i + 1) len
    | '|' -> disallow_next '#' s (i + 1) len
    | '"' | '(' | ')' | ';' | '\000'..'\032' | '\127'..'\255' -> false
    | _ -> jbuild s (i + 1) len
  and disallow_next c s i len =
    i = len || String.unsafe_get s i <> c && jbuild s i len
  in
  let rec dune s i len =
    i = len ||
    match String.unsafe_get s i with
    | '%' | '"' | '(' | ')' | ';' | '\000'..'\032' | '\127'..'\255' -> false
    | _ -> dune s (i + 1) len
  in
  let make looper s =
    let len = String.length s in
    len > 0 && looper s 0 len
  in
  (make jbuild, make dune)

let of_string s = A s

let to_string (A t) syntax =
  match syntax with
  | Jbuild ->
    if is_valid_jbuild t then
      t
    else
      invalid_argf "Dune atom '%s' cannot be printed" t
  | Dune ->
    if is_valid_dune t then
      t
    else
      invalid_argf "Jbuild atom '%s' cannot be printed" t

let of_int i = of_string (string_of_int i)
let of_float x = of_string (string_of_float x)
let of_bool x = of_string (string_of_bool x)
let of_digest d = of_string (Digest.to_hex d)
let of_int64 i = of_string (Int64.to_string i)
