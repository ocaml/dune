open Import

let pkg_name = "ocamlformat"
let binary_name = "ocamlformat"

let version_of_ocamlformat ocamlformat_config =
  Io.lines_of_file ocamlformat_config
  |> List.find_map ~f:(fun line ->
    match String.split_on_char ~sep:'=' line |> List.map ~f:String.trim with
    | [ "version"; value ] -> Some value
    | _ -> None)
;;

let pkg () =
  let ocamlformat = "ocamlformat" in
  let ocamlformat_config = Path.Source.of_string ".ocamlformat" |> Path.source in
  match Path.exists ocamlformat_config with
  | false -> ocamlformat, None
  | true -> ocamlformat, version_of_ocamlformat ocamlformat_config
;;
