open Import

type real =
  { oc  : out_channel
  ; buf : Buffer.t
  ; ppf : Format.formatter
  }

type t = real option

let no_log = None

let create () =
  if not (Sys.file_exists "_build") then
    Unix.mkdir "_build" 0o777;
  let oc = open_out_bin "_build/log" in
  Printf.fprintf oc "# %s\n%!"
    (String.concat (List.map (Array.to_list Sys.argv) ~f:quote_for_shell) ~sep:" ");
  let buf = Buffer.create 1024 in
  let ppf = Format.formatter_of_buffer buf in
  Some { oc; buf; ppf }

let info_internal { oc; _ } str =
  List.iter (String.split_lines str) ~f:(function
    | "" -> output_string oc "#\n"
    | s  -> Printf.fprintf oc "# %s\n" s);
  flush oc

let info t str =
  match t with
  | None -> ()
  | Some t -> info_internal t str

let infof t fmt =
  match t with
  | None -> Format.ikfprintf ignore Format.str_formatter fmt
  | Some t ->
    Format.kfprintf
      (fun ppf ->
         Format.pp_print_flush ppf ();
         let s = Buffer.contents t.buf in
         Buffer.clear t.buf;
         info_internal t s)
      t.ppf
      fmt

let command t ~command_line ~output ~exit_status =
  match t with
  | None -> ()
  | Some { oc; _ } ->
    Printf.fprintf oc "$ %s\n" (Ansi_color.strip command_line);
    List.iter (String.split_lines output) ~f:(fun s ->
      match Ansi_color.strip s with
      | "" -> output_string oc ">\n"
      | s  -> Printf.fprintf oc "> %s\n" s);
    (match (exit_status : Unix.process_status) with
     | WEXITED   0 -> ()
     | WEXITED   n -> Printf.fprintf oc "[%d]\n" n
     | WSIGNALED n -> Printf.fprintf oc "[got signal %s]\n" (Utils.signal_name n)
     | WSTOPPED  _ -> assert false);
    flush oc
