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

let daemonize dir f =
  if Unix.fork () = 0 then (
    ignore (Unix.setsid ());
    Sys.set_signal Sys.sighup Sys.Signal_ignore;
    Sys.set_signal Sys.sigpipe Sys.Signal_ignore;
    if Unix.fork () = 0 then (
      Unix.chdir dir;
      let null = open_in "/dev/null"
      and out = open_out "stdout"
      and err = open_out "stderr" in
      Unix.dup2
        (Unix.descr_of_in_channel null)
        (Unix.descr_of_in_channel stdin);
      Unix.dup2
        (Unix.descr_of_out_channel out)
        (Unix.descr_of_out_channel stdout);
      Unix.dup2
        (Unix.descr_of_out_channel err)
        (Unix.descr_of_out_channel stderr);
      close_in null;
      close_out out;
      close_out err;
      ignore (Unix.umask 0);
      f ()
    );
    exit 0
  )

module Modes = Map.Make (String)

let modes = Modes.empty

let start () =
  let port_path = ref (Dune_manager.default_port_file ())
  and root = ref (Dune_memory.default_root ())
  and foreground = ref false
  and usage = Printf.sprintf "start [OPTIONS]" in
  let report_endpoint () =
    let port_file = open_in (Path.to_string !port_path) in
    Printf.printf "listening on %s\n%!" (input_line port_file)
  in
  Arg.parse_argv ~current:Arg.current Sys.argv
    [ path_option "port" port_path "file to write listening port to"
    ; path_option "root" root "dune memory root"
    ; ("--foreground", Arg.Set foreground, "do not daemonize")
    ]
    (fun o -> raise (Arg.Bad (Printf.sprintf "unexpected option: %s" o)))
    usage;
  match Result.ok_exn (Dune_manager.check_port_file !port_path) with
  | None ->
      let f () =
        Option.iter ~f:Path.mkdir_p
          (Path.parent (Dune_manager.default_port_file ()));
        let f () =
          Console.init
            ( if !foreground then
              Verbose
            else
              Quiet );
          let log_file = Path.relative !root "log" in
          Log.init ~file:(This log_file) ();
          let manager = Dune_manager.make ~root:!root () in
          let port_f port =
            if make_port_file !port_path port = None then
              Dune_manager.stop manager
            else if !foreground then
              report_endpoint ()
          in
          Sys.set_signal Sys.sigint
            (Sys.Signal_handle (fun _ -> Dune_manager.stop manager));
          Sys.set_signal Sys.sigterm
            (Sys.Signal_handle (fun _ -> Dune_manager.stop manager));
          try
            let f () = Dune_manager.run ~port_f manager
            and finally () = Unix.truncate (Path.to_string !port_path) 0 in
            Exn.protect ~f ~finally
          with
          | Dune_manager.Error s ->
              Printf.fprintf stderr "%s: fatal error: %s\n%!" Sys.argv.(0) s;
              exit 1
          | Dune_manager.Stop ->
              ()
        and finally () = () in
        Exn.protect ~f ~finally
      in
      if !foreground then
        f ()
      else (
        daemonize (Path.to_string !root) f;
        let path = Path.to_string !port_path in
        let open Result.O in
        Result.ok_exn
          ( retry
              ~message:
                (Printf.sprintf "waiting for port file \"%s\" to be created"
                   path) (fun () ->
                try Some (Unix.openfile path [ Unix.O_RDONLY ] 0o600)
                with Unix.Unix_error (Unix.ENOENT, _, _) -> None)
          >>= fun fd ->
          retry
            ~message:
              (Printf.sprintf "waiting for port file \"%s\" to be locked" path)
            (fun () ->
              Option.some_if
                ( match Fcntl.lock_get fd Fcntl.Write with
                | Some (Fcntl.Read, _) ->
                    true
                | _ ->
                    false )
                ()) );
        report_endpoint ()
      )
  | Some (e, pid, _) ->
      if !foreground then
        User_error.raise [ Pp.textf "already running on %s (PID %i)" e pid ]
      else
        report_endpoint ()

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
