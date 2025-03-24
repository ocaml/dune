open Stdune

let command cmd args =
  let p = Unix.open_process_args_in cmd (Array.of_list (cmd :: args)) in
  let output =
    match Io.read_all_unless_large p with
    | Ok x -> x
    | Error () -> assert false
  in
  match Unix.close_process_in p with
  | WEXITED 0 -> Ok output
  | WEXITED n -> Error n
  | WSIGNALED _ | WSTOPPED _ -> assert false
;;

let () =
  let where = command "melc" [ "--where" ] in
  match where with
  | Error n ->
    Format.eprintf "error: %d@." n;
    exit 2
  | Ok where ->
    let parts =
      List.map (Bin.parse_path where) ~f:(fun part ->
        Format.asprintf "/MELC_STDLIB=%s" (part |> Path.parent_exn |> Path.to_string))
    in
    Format.printf "%s" (String.concat parts ~sep:":")
;;
