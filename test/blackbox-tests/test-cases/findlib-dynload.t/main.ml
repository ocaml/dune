let () = print_endline "m: init"

let () =
  for i = 1 to (Array.length Sys.argv - 1); do
    try
      Fl_dynload.load_packages [Sys.argv.(i)]
    with
    | Fl_package_base.No_such_package(pkg, _) ->
      Printf.printf "The package %S can't be found.\n%!" pkg
    | e ->
      (* This is just a convoluted way to avoid this module directly depending on
         Dynlink *)
      Mytool.Register.handle_dynlink_error e
  done
