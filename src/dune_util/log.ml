open Stdune
module Console = Dune_console

module File = struct
  type t =
    | Default
    | No_log_file
    | This of Path.t
    | Out_channel of out_channel
end

type real = { oc : out_channel option }

let t = Fdecl.create Dyn.opaque

let verbose = ref false

let init ?(file = File.Default) () =
  let oc =
    match file with
    | No_log_file -> None
    | Out_channel s -> Some s
    | This path -> Some (Io.open_out path)
    | Default ->
      Path.ensure_build_dir_exists ();
      Some (Io.open_out (Path.relative Path.build_dir "log"))
  in
  Option.iter oc ~f:(fun oc ->
      Printf.fprintf oc "# %s\n# OCAMLPARAM: %s\n%!"
        (String.concat
           (List.map (Array.to_list Sys.argv) ~f:String.quote_for_shell)
           ~sep:" ")
        (match Env.get Env.initial "OCAMLPARAM" with
        | Some s -> Printf.sprintf "%S" s
        | None -> "unset"));
  Fdecl.set t (Some { oc })

let init_disabled () = Fdecl.set t None

let t () = Fdecl.get t

let info_user_message msg =
  match t () with
  | None -> ()
  | Some { oc; _ } ->
    Option.iter oc ~f:(fun oc ->
        let s = Format.asprintf "%a@?" Pp.to_fmt (User_message.pp msg) in
        List.iter (String.split_lines s) ~f:(function
          | "" -> output_string oc "#\n"
          | s -> Printf.fprintf oc "# %s\n" s);
        flush oc);
    if !verbose then Console.print_user_message msg

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
    | WSIGNALED n -> Printf.fprintf oc "[got signal %s]\n" (Signal.name n)
    | WSTOPPED _ -> assert false);
    flush oc
