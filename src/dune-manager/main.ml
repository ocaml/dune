open Stdune
open Dune_manager

let runtime_dir =
  let xdg =
    try Sys.getenv "XDG_RUNTIME_DIR"
    with Not_found -> failwith "XDG_RUNTIME_DIR is not set"
  in
  Filename.concat xdg "dune-manager"

let path_option name var help =
  ( Printf.sprintf "--%s" name
  , Arg.String (fun p -> var := Path.of_string p)
  , Printf.sprintf "%s (default: %s)" help (Path.to_string !var) )

(* Check whether the PID file existes, and if so check the process is
still alive in case of a stale PID file*)

let check_pid_file p =
  try
    let p = Path.to_string p in
    let pid_file = open_in p in
    let f () =
      let pid = int_of_string (input_line pid_file) in
      try Unix.kill pid 0 ; Some pid
      with Unix.Unix_error (Unix.ESRCH, _, _) -> Unix.unlink p ; None
    and finally () = close_in pid_file in
    Exn.protect ~f ~finally
  with Sys_error _ -> None

let main () =
  let port_path = ref (Path.of_string (Filename.concat runtime_dir "port"))
  and pid_path = ref (Path.of_string (Filename.concat runtime_dir "pid"))
  and root = ref (Dune_memory.DuneMemory.default_root ())
  and usage = Printf.sprintf "%s [OPTIONS]" Sys.argv.(0) in
  Arg.parse_argv Sys.argv
    [ path_option "port" port_path "file to write listening port to"
    ; path_option "pid" pid_path
        "file to write PID to or check if the process is already running from"
    ; path_option "root" root "dune memory root" ]
    (fun o -> raise (Arg.Bad (Printf.sprintf "unexpected option: %s" o)))
    usage ;
  match check_pid_file !pid_path with
  | None ->
      Path.mkdir_p (Path.of_string runtime_dir) ;
      let pid_file = open_out (Path.to_string !pid_path) in
      let f () =
        output_string pid_file (string_of_int (Unix.getpid ())) ;
        close_out pid_file ;
        let manager = DuneManager.make ~root:!root ()
        and port_f port =
          let c = open_out (Path.to_string !port_path) in
          let f () = output_string c (string_of_int port)
          and finally () = close_out c in
          Exn.protect ~f ~finally
        in
        Sys.set_signal Sys.sigint
          (Sys.Signal_handle (fun _ -> DuneManager.stop manager)) ;
        try
          let f () = DuneManager.run ~port_f manager
          and finally () = Sys.remove (Path.to_string !port_path) in
          Exn.protect ~f ~finally
        with
        | DuneManager.Error s ->
            Printf.fprintf stderr "%s: fatal error: %s\n" Sys.argv.(0) s ;
            exit 1
        | DuneManager.Stop ->
            ()
      and finally () =
        close_out pid_file ;
        Unix.unlink (Path.to_string !pid_path)
      in
      Exn.protect ~f ~finally
  | Some pid ->
      Printf.fprintf stderr "%s: already running as PID %i\n" Sys.argv.(0) pid ;
      exit 0

let () =
  try main () with
  | Arg.Bad reason ->
      Printf.fprintf stderr "%s: command line error: %s" Sys.argv.(0) reason ;
      exit 1
  | Arg.Help help ->
      Printf.fprintf stdout "Usage: %s" help ;
      exit 1
