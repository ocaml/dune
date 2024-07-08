let pkg_tools = [ "ocamlformat" ]

let pkg_of_binary program =
  match program with
  | "ocamlformat" -> Some "ocamlformat"
  | _ -> None
;;
