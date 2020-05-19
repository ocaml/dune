open Stdune
open Import

let name = "cache"

let man =
  [ `S "DESCRIPTION"
  ; `P
      {|Dune is able to share build artifacts between workspaces.
        $(b,dune cache-daemon) is a daemon that runs in the background
        and manages this shared cache. For instance, it makes sure that it
        does not grow too big and try to maximise sharing between the various
        workspaces that are using the shared cache.|}
  ; `P
      {|The daemon is automatically started by Dune when the shared cache is
        enabled. You do not need to run this command manually.|}
  ; `S "ACTIONS"
  ; `P {|$(b,start) starts the daemon if not already running.|}
  ; `P {|$(b,stop) stops the daemon.|}
  ; `P {|$(b,trim) removes oldest files from the cache to free space.|}
  ; `Blocks Common.help_secs
  ]

let doc = "Manage the shared artifacts cache"

let info = Term.info name ~doc ~man

let start ~config ~foreground ~port_path ~root ~display =
  let show_endpoint ep =
    if display <> Some Config.Display.Quiet then Printf.printf "%s\n%!" ep
  in
  let f started =
    let started daemon_info =
      if foreground then show_endpoint daemon_info;
      started ~daemon_info
    in
    Log.verbose := foreground;
    Cache_daemon.daemon ~root ~config started
  in
  match Daemonize.daemonize ~workdir:root ~foreground port_path f with
  | Result.Ok Finished -> ()
  | Result.Ok (Daemonize.Started { daemon_info = endpoint; _ }) ->
    show_endpoint endpoint
  | Result.Ok (Daemonize.Already_running { daemon_info = endpoint; _ })
    when not foreground ->
    show_endpoint endpoint
  | Result.Ok (Daemonize.Already_running { daemon_info = endpoint; pid }) ->
    User_error.raise
      [ Pp.textf "already running on %s (PID %i)" endpoint (Pid.to_int pid) ]
  | Result.Error reason -> User_error.raise [ Pp.text reason ]

let stop ~port_path =
  match Daemonize.stop port_path with
  | Error s -> User_error.raise [ Pp.text s ]
  | Ok () -> ()

let trim ~trimmed_size ~size =
  Log.init_disabled ();
  let open Result.O in
  match
    let* cache =
      (* CR-soon amokhov: The [Hadrlink] duplication mode is chosen artitrarily
         here, instead of respecting the corresponding configuration setting,
         because the mode doesn't matter for the trimmer. It would be better to
         refactor the code to avoid such arbitrary choices. *)
      Cache.Local.make ~duplication_mode:Cache.Duplication_mode.Hardlink
        ~command_handler:ignore ()
    in
    let () =
      match Cache.Local.detect_unexpected_dirs_under_cache_root cache with
      | Ok [] -> ()
      | Ok dirs ->
        User_error.raise
          [ Pp.text "Unexpected directories found at the cache root:"
          ; Pp.enumerate dirs ~f:(fun dir -> Path.to_string dir |> Pp.text)
          ; Pp.text
              "These directories are probably used by Dune of a different \
               version. Please trim the cache manually."
          ]
      | Error e -> User_error.raise [ Pp.text (Unix.error_message e) ]
    in
    let+ goal =
      match (trimmed_size, size) with
      | Some trimmed_size, None -> Result.Ok trimmed_size
      | None, Some size ->
        Result.Ok (Int64.sub (Cache.Local.overhead_size cache) size)
      | _ -> Result.Error "specify either --size or --trimmed-size"
    in
    Cache.Local.trim cache ~goal
  with
  | Error s -> User_error.raise [ Pp.text s ]
  | Ok { trimmed_bytes } ->
    User_message.print
      (User_message.make [ Pp.textf "Freed %Li bytes" trimmed_bytes ])

type mode =
  | Start
  | Stop
  | Trim

let modes = [ ("start", Start); ("stop", Stop); ("trim", Trim) ]

let path_conv =
  ( (fun s -> `Ok (Path.of_string s))
  , fun fmt p -> Format.pp_print_string fmt (Path.to_string_maybe_quoted p) )

let term =
  Term.ret
  @@ let+ config = Common.config_term
     and+ mode =
       Arg.(
         value
         & pos 0 (some (enum modes)) None
         & info [] ~docv:"ACTION"
             ~doc:
               (Printf.sprintf "The cache-daemon action to perform (%s)"
                  (Arg.doc_alts_enum modes)))
     and+ foreground =
       Arg.(
         value & flag
         & info [ "foreground"; "f" ]
             ~doc:"Whether to start in the foreground or as a daemon")
     and+ exit_no_client =
       let doc = "Whether to exit once all clients have disconnected" in
       Arg.(
         value & flag
         & info [ "exit-no-client" ] ~doc
             ~env:(Arg.env_var "DUNE_CACHE_EXIT_NO_CLIENT" ~doc))
     and+ port_path =
       Arg.(
         value
         & opt path_conv (Cache_daemon.default_port_file ())
         & info ~docv:"PATH" [ "port-file" ]
             ~doc:"The file to read/write the daemon port from/to.")
     and+ root =
       Arg.(
         value
         & opt path_conv (Cache.Local.default_root ())
         & info ~docv:"PATH" [ "root" ] ~doc:"Root of the dune cache")
     and+ trimmed_size =
       Arg.(
         value
         & opt (some bytes) None
         & info ~docv:"BYTES" [ "trimmed-size" ]
             ~doc:"size to trim from the cache")
     and+ size =
       Arg.(
         value
         & opt (some bytes) None
         & info ~docv:"BYTES" [ "size" ] ~doc:"size to trim the cache to")
     and+ display = Common.display_term in
     match mode with
     | Some Start ->
       let config =
         { Cache_daemon.exit_no_client
         ; duplication_mode = config.cache_duplication
         }
       in
       `Ok (start ~config ~foreground ~port_path ~root ~display)
     | Some Stop -> `Ok (stop ~port_path)
     | Some Trim -> `Ok (trim ~trimmed_size ~size)
     | None -> `Help (`Pager, Some name)

let command = (term, info)
