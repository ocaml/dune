open! Stdune

type t =
  | C
  | Exe
  | Object
  | Shared_object
  | Js

let decode =
  let open Dune_lang.Decoder in
  sum
    [ ("c", Dune_lang.Syntax.since Stanza.syntax (1, 2) >>> return C)
    ; ("exe", return Exe)
    ; ("object", return Object)
    ; ("shared_object", return Shared_object)
    ; ("js", Dune_lang.Syntax.since Stanza.syntax (1, 11) >>> return Js)
    ]

let to_string = function
  | C -> "c"
  | Exe -> "exe"
  | Object -> "object"
  | Shared_object -> "shared_object"
  | Js -> "js"

let to_dyn t =
  let open Dyn.Encoder in
  constr (to_string t) []

let encode t = Dune_lang.unsafe_atom_of_string (to_string t)

let all = [ C; Exe; Object; Shared_object; Js ]
