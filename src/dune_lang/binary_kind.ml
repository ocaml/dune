open Stdune

type t =
  | C
  | Exe
  | Object
  | Shared_object
  | Plugin
  | Js

let compare x y =
  match x, y with
  | C, C -> Eq
  | C, _ -> Lt
  | _, C -> Gt
  | Exe, Exe -> Eq
  | Exe, _ -> Lt
  | _, Exe -> Gt
  | Object, Object -> Eq
  | Object, _ -> Lt
  | _, Object -> Gt
  | Shared_object, Shared_object -> Eq
  | Shared_object, _ -> Lt
  | _, Shared_object -> Gt
  | Plugin, Plugin -> Eq
  | Plugin, _ -> Lt
  | _, Plugin -> Gt
  | Js, Js -> Eq
;;

let decode =
  let open Dune_sexp in
  let open Decoder in
  sum
    [ "c", Syntax.since Stanza.syntax (1, 2) >>> return C
    ; "exe", return Exe
    ; "object", return Object
    ; "shared_object", return Shared_object
    ; "plugin", Syntax.since Stanza.syntax (2, 4) >>> return Plugin
    ; "js", Syntax.since Stanza.syntax (1, 11) >>> return Js
    ]
;;

let to_string = function
  | C -> "c"
  | Exe -> "exe"
  | Object -> "object"
  | Shared_object -> "shared_object"
  | Plugin -> "plugin"
  | Js -> "js"
;;

let to_dyn t = Dyn.variant (to_string t) []
let encode t = Dune_sexp.atom (to_string t)
let all = [ C; Exe; Object; Shared_object; Plugin; Js ]
