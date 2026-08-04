open Stdune

let read_instructions path =
  let instructions =
    Io.lines_of_file path
    |> List.find_map ~f:(fun line ->
      match String.split line ~on:';' with
      | count :: _unit :: event :: _
        when Option.is_some (String.drop_prefix event ~prefix:"instructions") ->
        Int.of_string count
      | _ -> None)
  in
  match instructions with
  | Some instructions -> instructions
  | None ->
    User_error.raise
      [ Pp.textf "Unable to read the instruction count from %s" (Path.to_string path) ]
;;
