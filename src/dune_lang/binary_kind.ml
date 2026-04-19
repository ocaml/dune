open Import

type t =
  | C
  | Exe
  | Object
  | Shared_object
  | Plugin

let repr =
  Repr.variant
    "binary-kind"
    [ Repr.case0 "c" ~test:(function
        | C -> true
        | _ -> false)
    ; Repr.case0 "exe" ~test:(function
        | Exe -> true
        | _ -> false)
    ; Repr.case0 "object" ~test:(function
        | Object -> true
        | _ -> false)
    ; Repr.case0 "shared_object" ~test:(function
        | Shared_object -> true
        | _ -> false)
    ; Repr.case0 "plugin" ~test:(function
        | Plugin -> true
        | _ -> false)
    ]
;;

let _, compare = Repr.make_compare repr

let decode =
  let open Decoder in
  sum
    [ "c", Syntax.since Stanza.syntax (1, 2) >>> return C
    ; "exe", return Exe
    ; "object", return Object
    ; "shared_object", return Shared_object
    ; "plugin", Syntax.since Stanza.syntax (2, 4) >>> return Plugin
    ]
;;

let to_string = function
  | C -> "c"
  | Exe -> "exe"
  | Object -> "object"
  | Shared_object -> "shared_object"
  | Plugin -> "plugin"
;;

let to_dyn = Repr.to_dyn repr
let encode t = Dune_sexp.atom (to_string t)
let all = [ C; Exe; Object; Shared_object; Plugin ]
