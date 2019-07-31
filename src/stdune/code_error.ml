type t =
  { message : string
  ; data : (string * Dyn.t) list
  ; loc : Loc0.t option
  }

exception E of t

let raise ?loc message data =
  raise (E { message; data ; loc })

let to_dyn { loc = _; message; data } : Dyn.t =
  Tuple
    [ String message
    ; Record data
    ]

let () =
  Printexc.register_printer (function
    | E t -> Some (Dyn.to_string (to_dyn t))
    | _ -> None)
