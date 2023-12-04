type t =
  { message : string
  ; data : (string * Dyn.t) list
  ; loc : Loc0.t option
  }

exception E of t

let create ?loc message data = { message; data; loc }
let raise ?loc message data = raise (E { message; data; loc })

let dyn_fields_without_loc { loc = _; message; data } =
  [ Dyn.String message; Record data ]
;;

let to_dyn_without_loc t : Dyn.t = Tuple (dyn_fields_without_loc t)

let to_dyn t : Dyn.t =
  let fields = dyn_fields_without_loc t in
  let fields =
    match t.loc with
    | None -> fields
    | Some loc -> Loc0.to_dyn loc :: fields
  in
  Tuple fields
;;

let () =
  Printexc.register_printer (function
    | E t -> Some (Dyn.to_string (to_dyn t))
    | _ -> None)
;;
