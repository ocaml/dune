open Dune_action_plugin.V1

let path = Path.of_string "some_file2"

let action =
  let open Dune_action_plugin.V1.O in
  write_file ~path ~data:"Hello from some_file2!"
  |> stage ~f:(fun () ->
    let+ data = read_file ~path in
    print_endline data)
;;

let () = run action
