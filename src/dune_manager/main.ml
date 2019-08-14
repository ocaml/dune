open Stdune
open Dune_manager.Utils

let path_option name var help =
  ( Printf.sprintf "--%s" name
  , Arg.String (fun p -> var := Path.of_string p)
  , Printf.sprintf "%s (default: %s)" help (Path.to_string !var) )

let make_port_file path contents =
  Option.iter ~f:Path.mkdir_p (Path.parent path);
  let p = Path.to_string path
  and length = String.length contents in
  let fd = Unix.openfile p [ Unix.O_RDWR; Unix.O_CREAT ] 0o600 in
  let cancel () = Unix.close fd in
  if Fcntl.lock_try fd Fcntl.Write then
    if
      (* Write the content to the file. *)
      Unix.write fd (Bytes.of_string contents) 0 length <> length
    then (
      cancel ();
      User_error.raise
        [ Pp.textf "couldn't write whole endpoint to port file \"%s\"" p ]
    ) else (
      Fcntl.lock fd Fcntl.Read;
      Some fd
    )
  else
    None

module Modes = Map.Make (String)

let modes = Modes.empty

let start () =
  let show_endpoint ep = Printf.printf "listening on %s\n%!" ep in
  let config =
    let get item =
      try Some (Sys.getenv ("DUNE_CACHE_" ^ String.uppercase item))
      with Not_found -> None
    in
    let bool item = Option.is_some (get item) in
    { Dune_manager.exit_no_client = bool "exit_no_client" }
  in
  let port_path = ref (Dune_manager.default_port_file ())
  and root = ref (Dune_memory.default_root ())
  and foreground = ref false
  and usage = Printf.sprintf "start [OPTIONS]" in
  Arg.parse_argv ~current:Arg.current Sys.argv
    [ path_option "port" port_path "file to write listening port to"
    ; path_option "root" root "dune memory root"
    ; ("--foreground", Arg.Set foreground, "do not daemonize")
    ]
    (fun o -> raise (Arg.Bad (Printf.sprintf "unexpected option: %s" o)))
    usage;
  let root = !root in
  let f started =
    let started content =
      if !foreground then show_endpoint content;
      started content
    in
    Console.init
      ( if !foreground then
        Verbose
      else
        Quiet );
    Dune_manager.daemon ~root ~config started
  in
  match
    Daemonize.daemonize ~workdir:root ~foreground:!foreground !port_path f
  with
  | Result.Ok Finished ->
      ()
  | Result.Ok (Daemonize.Started (endpoint, _)) ->
      show_endpoint endpoint
  | Result.Ok (Daemonize.Already_running (endpoint, _)) when not !foreground ->
      show_endpoint endpoint
  | Result.Ok (Daemonize.Already_running (endpoint, pid)) ->
      User_error.raise
        [ Pp.textf "already running on %s (PID %i)" endpoint pid ]
  | Result.Error reason ->
      User_error.raise [ Pp.text reason ]

let modes = Modes.add_exn modes "start" start

let stop () =
  let port_path = ref (Dune_manager.default_port_file ()) in
  Arg.parse_argv ~current:Arg.current Sys.argv
    [ path_option "port" port_path "file to read listening port from" ]
    (fun o -> raise (Arg.Bad (Printf.sprintf "unexpected option: %s" o)))
    "stop [OPTIONS]";
  match
    Result.ok_exn (Dune_manager.check_port_file ~close:false !port_path)
  with
  | None ->
      User_error.raise [ Pp.textf "not running" ]
  | Some (_, pid, fd) ->
      Unix.kill pid Sys.sigterm;
      Result.ok_exn
        (retry
           ~message:(Printf.sprintf "waiting for daemon to stop (PID %i)" pid)
           (fun () -> Option.some_if (Fcntl.lock_get fd Fcntl.Write = None) ()))

let modes = Modes.add_exn modes "stop" stop

let main () =
  let nargs = Array.length Sys.argv
  and help =
    Printf.sprintf "[--help%s]\n"
      (Modes.foldi modes ~init:"" ~f:(fun k _ b -> b ^ "|" ^ k))
  in
  Arg.current := 1;
  if nargs = 1 || Sys.argv.(1) = "--help" then
    raise (Arg.Help help)
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
      Printf.fprintf stderr "%s: command line error: %s\n%!" Sys.argv.(0)
        reason;
      exit 1
  | User_error.E msg ->
      Printf.fprintf stderr "%s: user error: %s\n" Sys.argv.(0)
        (Format.asprintf "%a@?" Pp.render_ignore_tags (User_message.pp msg));
      exit 2
  | Failure reason ->
      Printf.fprintf stderr "%s: fatal error: %s\n%!" Sys.argv.(0) reason;
      exit 3
  | Arg.Help help ->
      Printf.fprintf stdout "Usage: %s %s\n%!" Sys.argv.(0) help;
      exit 0
