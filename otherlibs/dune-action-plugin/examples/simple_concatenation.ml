open Dune_action_plugin.V1

let action =
  let source1 = read_file ~path:(Path.of_string "source1")
  and source2 = read_file ~path:(Path.of_string "source2") in
  both source1 source2
  |> stage ~f:(fun (source1, source2) ->
    let data = source1 ^ source2 in
    write_file ~path:(Path.of_string "target") ~data)
;;

let () = run action
