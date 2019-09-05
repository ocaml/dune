open Dune_action

let action =
  read_file ~path:(Path.of_string "bar_source")
  |> stage ~f:(fun data ->
    let data = Result.get_ok data in
    write_file ~path:(Path.of_string "bar_copy") ~data
    |> map ~f:(fun result -> Result.get_ok result))

let () = run action
