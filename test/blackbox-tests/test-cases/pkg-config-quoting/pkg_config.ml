let () =
  let args = List.tl (Array.to_list Sys.argv) in
  Format.printf "%a@."
    (Format.pp_print_list Format.pp_print_string) args
