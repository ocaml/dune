let () =
  match Sys.backend_type with
    Other typ -> print_endline typ
  | _ -> assert false
