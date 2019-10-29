open Stdune
open Import

let name = "cache"

let man =
  [ `S "DESCRIPTION"
  ; `P
      {|Dune allows to share build artifacts between workspaces.
        $(b,dune cache-daemon) is a daemon that runs in the background
        and manages this shared cache. For instance, it makes sure that it
        does not grow too big and try to maximise sharing between the various
        workspace that are using the shared cache.|}
  ; `P
      {|The daemon is automatically started by Dune when the shared cache is
        enabled. You do not need to run this command manually.|}
  ; `S "ACTIONS"
  ; `P {|$(b,start) starts the daemon if not already running.|}
  ; `P {|$(b,stop) stops the daemon.|}
  ; `P {|$(b,trim) remove oldest files from the cache to free space.|}
  ; `Blocks Common.help_secs
  ]

let doc = "Manage the shared artifacts cache"

let info = Term.info name ~doc ~man

let start ~exit_no_client ~foreground ~port_path ~root =
  let show_endpoint ep = Printf.printf "listening on %s\n%!" ep in
  let config = { Dune_manager.exit_no_client } in
  let f started =
    let started content =
      if foreground then show_endpoint content;
      started content
    in
    Console.init
      ( if foreground then
        Verbose
      else
        Quiet );
    Dune_manager.daemon ~root ~config started
  in
  match Daemonize.daemonize ~workdir:root ~foreground port_path f with
  | Result.Ok Finished -> ()
  | Result.Ok (Daemonize.Started (endpoint, _)) -> show_endpoint endpoint
  | Result.Ok (Daemonize.Already_running (endpoint, _)) when not foreground ->
    show_endpoint endpoint
  | Result.Ok (Daemonize.Already_running (endpoint, pid)) ->
    User_error.raise [ Pp.textf "already running on %s (PID %i)" endpoint pid ]
  | Result.Error reason -> User_error.raise [ Pp.text reason ]

let stop ~port_path =
  match Daemonize.stop port_path with
  | Error s -> User_error.raise [ Pp.text s ]
  | Ok () -> ()

let trim ~trimmed_size ~size =
  Log.init_disabled ();
  let open Result.O in
  match
    let* memory = Dune_memory.Memory.make (fun _ -> ()) in
    let+ trimmed_size =
      match (trimmed_size, size) with
      | Some trimmed_size, None -> Result.Ok trimmed_size
      | None, Some size -> Result.Ok (Dune_memory.size memory - size)
      | _ -> Result.Error "specify either --size either --trimmed-size"
    in
    Dune_memory.trim memory trimmed_size
  with
  | Error s -> User_error.raise [ Pp.text s ]
  | Ok (size, _) ->
    User_message.print (User_message.make [ Pp.textf "Freed %i bytes" size ])

type mode =
  | Start
  | Stop
  | Trim

let modes = [ ("start", Start); ("stop", Stop); ("trim", Trim) ]

let path_conv = ((fun s -> `Ok (Path.of_string s)), Path.pp)

let term =
  Term.ret
  @@ let+ mode =
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
             ~doc:"Whether to start in the foreground or as a daeon")
     and+ exit_no_client =
       let doc = "Whether to exit once all clients have disconnected" in
       Arg.(
         value & flag
         & info [ "exit-no-client" ] ~doc
             ~env:(Arg.env_var "DUNE_CACHE_EXIT_NO_CLIENT" ~doc))
     and+ port_path =
       Arg.(
         value
         & opt path_conv (Dune_manager.default_port_file ())
         & info ~docv:"PATH" [ "port-file" ]
             ~doc:"The file to read/write the daemon port to/from.")
     and+ root =
       Arg.(
         value
         & opt path_conv (Dune_memory.default_root ())
         & info ~docv:"PATH" [ "root" ] ~doc:"Root of the dune cache")
     and+ trimmed_size =
       Arg.(
         value
         & opt (some int) None
         & info ~docv:"BYTES" [ "trimmed-size" ]
             ~doc:"size to trim from the cache")
     and+ size =
       Arg.(
         value
         & opt (some int) None
         & info ~docv:"BYTES" [ "size" ] ~doc:"size to trim the cache to")
     in
     match mode with
     | Some Start -> `Ok (start ~exit_no_client ~foreground ~port_path ~root)
     | Some Stop -> `Ok (stop ~port_path)
     | Some Trim -> `Ok (trim ~trimmed_size ~size)
     | None -> `Help (`Pager, Some name)

let command = (term, info)
