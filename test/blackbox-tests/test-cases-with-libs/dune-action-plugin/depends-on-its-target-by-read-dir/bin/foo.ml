open Dune_action_plugin

let contains equal list elem = List.find list (equal elem) |> Option.is_some

let action =
  let open Dune_action_plugin.O in
  let+ _ = read_directory ~path:(Path.of_string ".")
  and+ _ =
    write_file ~path:(Path.of_string "some_file") ~data:"Hello from some_file!"
  in
  ()

let () = run action
