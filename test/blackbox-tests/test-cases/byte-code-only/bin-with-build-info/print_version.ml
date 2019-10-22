let () =
  (match Build_info.Build_info_data.version with
   | None -> print_endline "<version missing>"
   | Some version -> print_endline version)
