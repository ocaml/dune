open Stdune

let command cmd args =
  let p = Unix.open_process_args_in cmd (Array.of_list (cmd :: args)) in
  let output =
    match Fs_io.read_all_unless_large p with
    | Ok x -> x
    | Error exn -> raise exn
  in
  match Unix.close_process_in p with
  | WEXITED 0 -> Ok output
  | WEXITED n -> Error n
  | WSIGNALED _ | WSTOPPED _ -> assert false
;;

let normalize_separators path =
  let len = String.length path in
  let buf = Buffer.create len in
  let rec loop i prev_was_sep =
    if i = len
    then Buffer.contents buf
    else (
      let c = path.[i] in
      if c = '/'
      then (
        if not prev_was_sep then Buffer.add_char buf c;
        loop (i + 1) true)
      else (
        Buffer.add_char buf c;
        loop (i + 1) false))
  in
  loop 0 false
;;

let () =
  let where = command "melc" [ "--where" ] in
  match where with
  | Error n ->
    Format.eprintf "error: %d@." n;
    exit 2
  | Ok where ->
    let parts =
      List.concat_map (Bin.parse_path where) ~f:(fun part ->
        let path = part |> Path.parent_exn |> Path.to_string in
        let normalized = normalize_separators path in
        let paths =
          if String.equal path normalized then [ path ] else [ path; normalized ]
        in
        List.map paths ~f:(fun path -> Format.asprintf "/MELC_STDLIB=%s" path))
    in
    Format.printf "%s" (String.concat parts ~sep:":")
;;
