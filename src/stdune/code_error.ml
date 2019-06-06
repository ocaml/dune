module Dyn = Dyn0

type t =
  { message : string
  ; data : (string * Dyn.t) list
  }

exception E of t

let raise message data =
  raise (E { message; data })

let to_dyn { message; data } : Dyn.t =
  Tuple
    [ String message
    ; Record data
    ]

let () =
  Printexc.register_printer (function
    | E t -> Some (Dyn.to_string (to_dyn t))
    | _ -> None)
