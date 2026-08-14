open Stdune

let with_color env =
  Env.update env ~var:Env.Var._OCAMLPARAM ~f:(function
    | None -> Some "color=always,_"
    | Some s -> Some ("color=always," ^ s))
;;

let caml_ld_library_path = Env.Var.of_string "CAML_LD_LIBRARY_PATH"
