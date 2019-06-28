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

let daemonize dir f =
  if Unix.fork () = 0 then (
    ignore (Unix.setsid ()) ;
    Sys.set_signal Sys.sighup Sys.Signal_ignore ;
    Sys.set_signal Sys.sigpipe Sys.Signal_ignore ;
    if Unix.fork () = 0 then (
      Unix.chdir dir ;
      let null = open_in "/dev/null"
      and out = open_out "stdout"
      and err = open_out "stderr" in
      Unix.dup2
        (Unix.descr_of_in_channel null)
        (Unix.descr_of_in_channel stdin) ;
      Unix.dup2
        (Unix.descr_of_out_channel out)
        (Unix.descr_of_out_channel stdout) ;
      Unix.dup2
        (Unix.descr_of_out_channel err)
        (Unix.descr_of_out_channel stderr) ;
      close_in null ;
      close_out out ;
      close_out err ;
      ignore (Unix.umask 0) ;
      f () ) ;
    exit 0 )

module Modes = Map.Make (String)

let modes = Modes.empty

let start () =
  let port_path = ref (Path.of_string (Filename.concat runtime_dir "port"))
  and pid_path = ref (Path.of_string (Filename.concat runtime_dir "pid"))
  and root = ref (Dune_memory.DuneMemory.default_root ())
  and foreground = ref false
  and usage = Printf.sprintf "start [OPTIONS]" in
  let report_endpoint () =
    let port_file = open_in (Path.to_string !port_path) in
    Printf.printf "listening on %s\n%!" (input_line port_file)
  in
  Arg.parse_argv ~current:Arg.current Sys.argv
    [ path_option "port" port_path "file to write listening port to"
    ; path_option "pid" pid_path
        "file to write PID to or check if the process is already running from"
    ; path_option "root" root "dune memory root"
    ; ("--foreground", Arg.Set foreground, "do not daemonize") ]
    (fun o -> raise (Arg.Bad (Printf.sprintf "unexpected option: %s" o)))
    usage ;
  match check_pid_file !pid_path with
  | None ->
      let f () =
        Path.mkdir_p (Path.of_string runtime_dir) ;
        let pid_file = open_out (Path.to_string !pid_path) in
        let f () =
          output_string pid_file (string_of_int (Unix.getpid ())) ;
          close_out pid_file ;
          let manager = DuneManager.make ~root:!root ()
          and port_f port =
            let c = open_out (Path.to_string !port_path) in
            let f () = output_string c port and finally () = close_out c in
            Exn.protect ~f ~finally ;
            if !foreground then report_endpoint ()
          in
          Sys.set_signal Sys.sigint
            (Sys.Signal_handle (fun _ -> DuneManager.stop manager)) ;
          Sys.set_signal Sys.sigterm
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
      in
      if !foreground then f ()
      else (
        daemonize (Path.to_string !root) f ;
        while not (Sys.file_exists (Path.to_string !port_path)) do
          Unix.sleepf 0.1
        done ;
        report_endpoint () )
  | Some pid ->
      if !foreground then
        failwith (Printf.sprintf "already running as PID %i\n" pid)
      else report_endpoint ()

let modes = Modes.add_exn modes "start" start

let stop () =
  let pid_path = ref (Path.of_string (Filename.concat runtime_dir "pid")) in
  Arg.parse_argv ~current:Arg.current Sys.argv
    [ path_option "pid" pid_path
        "file to write PID to or check if the process is already running from"
    ]
    (fun o -> raise (Arg.Bad (Printf.sprintf "unexpected option: %s" o)))
    "stop [OPTIONS]" ;
  match check_pid_file !pid_path with
  | None ->
      failwith "not running"
  | Some pid ->
      Printf.printf "kill %i\n%!" pid ;
      Unix.kill pid Sys.sigterm ;
      while try Unix.kill pid 0 ; true with _ -> false do
        Unix.sleepf 0.1
      done

let modes = Modes.add_exn modes "stop" stop

let main () =
  let nargs = Array.length Sys.argv
  and help =
    Printf.sprintf "[--help%s]\n"
      (Modes.foldi modes ~init:"" ~f:(fun k _ b -> b ^ "|" ^ k))
  in
  Arg.current := 1 ;
  if nargs = 1 || Sys.argv.(1) = "--help" then raise (Arg.Help help)
  else
    match Modes.find modes Sys.argv.(1) with
    | Some f ->
        f ()
    | None ->
        raise
          (Arg.Bad
             (Printf.sprintf "unknown mode \"%s\".\nUsage: %s %s" Sys.argv.(1)
                Sys.argv.(0) help))

let () =
  try main () with
  | Arg.Bad reason ->
      Printf.fprintf stderr "%s: command line error: %s" Sys.argv.(0) reason ;
      exit 1
  | Failure reason ->
      Printf.fprintf stderr "%s: fatal error: %s" Sys.argv.(0) reason ;
      exit 1
  | Arg.Help help ->
      Printf.fprintf stdout "Usage: %s %s" Sys.argv.(0) help ;
      exit 0
