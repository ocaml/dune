type t =
  { message : string
  ; data : (string * Dyn.t) list
  ; loc : Loc0.t option
  }

exception E of t

let raise ?loc message data =
  raise (E { message; data ; loc })

let to_dyn { loc; message; data } : Dyn.t =
  let fields = [Dyn.String message; Record data] in
  let fields =
    match loc with
    | None -> fields
    | Some loc -> Loc0.to_dyn loc :: fields
  in
  Tuple fields

let () =
  Printexc.register_printer (function
    | E t -> Some (Dyn.to_string (to_dyn t))
    | _ -> None)
