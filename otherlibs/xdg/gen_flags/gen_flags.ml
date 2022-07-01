let () =
  match Sys.argv.(1) with
  | "Win32" -> print_endline "(-lshell32 -lole32)"
  | _ -> print_endline "()"
