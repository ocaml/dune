open Stdune

let with_color env =
  Env.update env ~var:"OCAMLPARAM" ~f:(function
    | None -> Some "color=always,_"
    | Some s -> Some ("color=always," ^ s))
