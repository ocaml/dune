open Import

let version_of_ocamlformat_config ocamlformat_config =
  Io.lines_of_file ocamlformat_config
  |> List.find_map ~f:(fun line ->
    match String.split_on_char ~sep:'=' line |> List.map ~f:String.trim with
    | [ "version"; value ] -> Some (Package_version.of_string value)
    | _ -> None)
;;

let version_of_current_project's_ocamlformat_config () =
  let ocamlformat_config = Path.Source.of_string ".ocamlformat" |> Path.source in
  match Path.exists ocamlformat_config with
  | false -> None
  | true -> version_of_ocamlformat_config ocamlformat_config
;;
