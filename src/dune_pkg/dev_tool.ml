open Import

type cmd = Fmt

let pkg_tools = [ "ocamlformat" ]

let pkg_of_binary program =
  match program with
  | "ocamlformat" -> Some "ocamlformat"
  | _ -> None
;;

let version_of_ocamlformat ocamlformat_config =
  Io.lines_of_file ocamlformat_config
  |> List.find_map ~f:(fun line ->
    match String.split_on_char ~sep:'=' line |> List.map ~f:String.trim with
    | [ "version"; value ] -> Some value
    | _ -> None)
;;

let pkg_of_cmd = function
  | Fmt ->
    let ocamlformat = "ocamlformat" in
    let ocamlformat_config = Path.Source.of_string ".ocamlformat" |> Path.source in
    (match Path.exists ocamlformat_config with
     | false -> ocamlformat, None
     | true -> ocamlformat, version_of_ocamlformat ocamlformat_config)
;;
