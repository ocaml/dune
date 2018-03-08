type t =
  | Exe
  | Object
  | Shared_object

let t =
  let open Sexp.Of_sexp in
  enum
    [ "exe"           , Exe
    ; "object"        , Object
    ; "shared_object" , Shared_object
    ]

let all = [Exe; Object; Shared_object]
