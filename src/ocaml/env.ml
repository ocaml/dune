open Stdune

let with_color env =
  Env.update env ~var:Env.Var._OCAMLPARAM ~f:(function
    | None -> Some "color=always,_"
    | Some s -> Some ("color=always," ^ s))
;;

let with_gc_defaults env ~version =
  let ocamlrunparam = Env.Var.of_string "OCAMLRUNPARAM" in
  let camlrunparam = Env.Var.of_string "CAMLRUNPARAM" in
  if
    Version.supports_small_heap_limit version
    && (not (Env.mem env ~var:ocamlrunparam))
    && not (Env.mem env ~var:camlrunparam)
  then Env.add env ~var:ocamlrunparam ~value:"Xsmall_heap_limit=16777216"
  else env
;;

let caml_ld_library_path = Env.Var.of_string "CAML_LD_LIBRARY_PATH"
