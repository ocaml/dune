module File = struct
  type t =
    | Default
    | No_log_file
    | This of Path.t
end

type real =
  { oc : out_channel option
  ; buf : Buffer.t
  ; ppf : Format.formatter
  }

let t = Fdecl.create Dyn.Encoder.opaque

let init ?(file = File.Default) () =
  let oc =
    match file with
    | No_log_file -> None
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
        ( match Env.get Env.initial "OCAMLPARAM" with
        | Some s -> Printf.sprintf "%S" s
        | None -> "unset" ));
  let buf = Buffer.create 1024 in
  let ppf = Format.formatter_of_buffer buf in
  Fdecl.set t (Some { oc; buf; ppf })

let init_disabled () = Fdecl.set t None

let t () = Fdecl.get t

let info_internal { oc; _ } str =
  let write ppf =
    List.iter (String.split_lines str) ~f:(function
      | "" -> Format.pp_print_string ppf "#\n"
      | s -> Format.fprintf ppf "# %s\n" s);
    Format.pp_print_flush ppf ()
  in
  Option.iter ~f:(fun o -> write (Format.formatter_of_out_channel o)) oc;
  match Console.display () with
  | Verbose -> Console.print (Format.asprintf "%t" write)
  | _ -> ()

let info s =
  match t () with
  | None -> ()
  | Some t -> info_internal t s

let infof fmt =
  match t () with
  | None -> Format.ikfprintf ignore Format.str_formatter fmt
  | Some t ->
    Buffer.clear t.buf;
    Format.kfprintf
      (fun ppf ->
        Format.pp_print_flush ppf ();
        let s = Buffer.contents t.buf in
        info_internal t s)
      t.ppf fmt

let command ~command_line ~output ~exit_status =
  match t () with
  | None
  | Some { oc = None; _ } ->
    ()
  | Some { oc = Some oc; _ } ->
    Printf.fprintf oc "$ %s\n" (Ansi_color.strip command_line);
    List.iter (String.split_lines output) ~f:(fun s ->
        match Ansi_color.strip s with
        | "" -> output_string oc ">\n"
        | s -> Printf.fprintf oc "> %s\n" s);
    ( match (exit_status : Unix.process_status) with
    | WEXITED 0 -> ()
    | WEXITED n -> Printf.fprintf oc "[%d]\n" n
    | WSIGNALED n -> Printf.fprintf oc "[got signal %s]\n" (Signal.name n)
    | WSTOPPED _ -> assert false );
    flush oc
