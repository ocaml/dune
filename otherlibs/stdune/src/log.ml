module File = struct
  type t =
    | Default
    | No_log_file
    | Stderr

  let equal a b =
    match a, b with
    | Default, Default | No_log_file, No_log_file | Stderr, Stderr -> true
    | _, _ -> false
  ;;
end

type real = { oc : out_channel option }

let t = Fdecl.create Dyn.opaque
let verbose = ref false

let init (file : File.t) =
  let oc =
    match file with
    | No_log_file -> None
    | Stderr -> Some stderr
    | Default ->
      Path.ensure_build_dir_exists ();
      Some (Io.open_out (Path.relative Path.build_dir "log"))
  in
  Option.iter oc ~f:(fun oc ->
    Printf.fprintf
      oc
      "# %s\n# OCAMLPARAM: %s\n%!"
      (String.quote_list_for_shell (Array.to_list Sys.argv))
      (match Env.get Env.initial "OCAMLPARAM" with
       | Some s -> Printf.sprintf "%S" s
       | None -> "unset"));
  Fdecl.set t (Some { oc })
;;

let t () = Fdecl.get t

let log_oc oc msg =
  let s = Format.asprintf "%a@?" Pp.to_fmt (User_message.pp msg) in
  List.iter (String.split_lines s) ~f:(function
    | "" -> output_string oc "#\n"
    | s -> Printf.fprintf oc "# %s\n" s);
  flush oc
;;

let log msg =
  match t () with
  | None -> ()
  | Some { oc; _ } ->
    Option.iter oc ~f:(fun oc -> User_message.make (msg ()) |> log_oc oc)
;;

let forward_verbose = Fdecl.create Dyn.opaque
let set_forward_verbose = Fdecl.set forward_verbose

let info_user_message msg =
  match t () with
  | None -> ()
  | Some { oc; _ } ->
    Option.iter oc ~f:(fun oc -> log_oc oc msg);
    if !verbose then (Fdecl.get forward_verbose) msg
;;

let info paragraphs = info_user_message (User_message.make paragraphs)

let command ~command_line ~output ~exit_status =
  match t () with
  | None | Some { oc = None; _ } -> ()
  | Some { oc = Some oc; _ } ->
    Printf.fprintf oc "$ %s\n" (Ansi_color.strip command_line);
    List.iter (String.split_lines output) ~f:(fun s ->
      match Ansi_color.strip s with
      | "" -> output_string oc ">\n"
      | s -> Printf.fprintf oc "> %s\n" s);
    (match (exit_status : Unix.process_status) with
     | WEXITED 0 -> ()
     | WEXITED n -> Printf.fprintf oc "[%d]\n" n
     | WSIGNALED n ->
       let name = Signal.of_int n |> Signal.name in
       Printf.fprintf oc "[got signal %s]\n" name
     | WSTOPPED _ -> assert false);
    flush oc
;;
