type t = A of string [@@unboxed]

let invalid_argf fmt = Printf.ksprintf invalid_arg fmt

type syntax = Jbuild | Dune

let string_of_syntax = function
  | Jbuild -> "jbuild"
  | Dune -> "dune"

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

let of_string syn s =
  match syn with
  | Jbuild when is_valid_jbuild s -> Some (A s)
  | Dune when is_valid_dune s -> Some (A s)
  | _ -> None

let of_string_exn syn s =
  match of_string syn s with
  | Some s -> s
  | None ->
    invalid_argf "'%s' is not a valid %s atom" s (string_of_syntax syn)

let to_string (A t) syntax =
  match syntax with
  | Jbuild -> t
  | Dune ->
    if is_valid_dune t then
      t
    else
      invalid_argf "Jbuild atom '%s' is not a valid dune atom" t

let of_int i = of_string_exn Dune (string_of_int i)
let of_float x = of_string_exn Dune (string_of_float x)
let of_bool x = of_string_exn Dune (string_of_bool x)
let of_int64 i = of_string_exn Dune (Int64.to_string i)
let of_digest d = of_string_exn Dune (Digest.to_hex d)
