open Stdune

let source_path_of_string path =
  if Filename.is_relative path
  then Path.Source.(relative root path)
  else (
    let source_root = Path.to_absolute_filename (Path.source Path.Source.root) in
    match String.drop_prefix path ~prefix:source_root with
    | None -> User_error.raise [ Pp.textf "path isn't available in workspace" ]
    | Some s ->
      let s = String.drop_prefix_if_exists s ~prefix:"/" in
      Path.Source.(relative root s))
;;
