let () =
  match Sys.getenv "FOO" with
  | exception _ -> print_endline "None"
  | str -> Printf.printf "Some %S\n" str
;;
