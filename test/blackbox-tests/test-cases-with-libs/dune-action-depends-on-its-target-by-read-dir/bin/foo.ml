open Dune_action

let contains equal list elem = List.find list (equal elem) |> Option.is_some

let action =
  let read = read_directory ~path:(Path.of_string ".")
  and write =
    write_file ~path:(Path.of_string "bar") ~data:"Hello from bar!"
  in
  both read write |> map ~f:ignore

let () = run action
