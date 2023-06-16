open Import
include Path.Source

let required_extension = ".lock"

let is_valid_source_path path_source =
  String.equal (Path.Source.extension path_source) required_extension

let of_path_source path_source =
  if is_valid_source_path path_source then Ok path_source
  else
    Error
      (`Msg
        (sprintf "lockdir path %s does not have required extension \"%s\""
           (Path.Source.to_string_maybe_quoted path_source)
           required_extension))

let of_path_source_exn path_source =
  match of_path_source path_source with
  | Ok t -> t
  | Error (`Msg msg) -> User_error.raise [ Pp.text msg ]

let to_path_source t = t

let to_path = Path.source
