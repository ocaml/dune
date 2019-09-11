open Dune_action_plugin

let action =
  write_file
    ~path:(Path.of_string "../some_file")
    ~data:"Hello from some_file!"

let () = run action
