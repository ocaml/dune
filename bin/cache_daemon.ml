open Stdune
open Import

let name = "cache-daemon"

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
  ; `Blocks Common.help_secs
  ]

let doc = "Manage the shared artifacts cache"

let info = Term.info name ~doc ~man

let retry ?message ?(count = 100) f =
  let rec loop = function
    | x when x >= count ->
      Result.Error
        (Failure
           ( Printf.sprintf "too many retries (%i)" x
           ^
           match message with
           | None -> ""
           | Some msg -> ": " ^ msg ))
    | x -> (
      match f () with
      | Some v -> Result.Ok v
      | None ->
        Thread.delay 0.1;
        loop (x + 1) )
  in
  loop 0

let start ~port_path ~root ~foreground =
  let show_endpoint ep = Printf.printf "listening on %s\n%!" ep in
  let config =
    let get item =
      try Some (Sys.getenv ("DUNE_CACHE_" ^ String.uppercase item))
      with Not_found -> None
    in
    let bool item = Option.is_some (get item) in
    { Dune_manager.exit_no_client = bool "exit_no_client" }
  in
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
  match
    Result.ok_exn (Dune_manager.check_port_file ~close:false port_path)
  with
  | None -> User_error.raise [ Pp.textf "not running" ]
  | Some (_, pid, fd) ->
    Unix.kill pid Sys.sigterm;
    Result.ok_exn
      (retry
         ~message:(Printf.sprintf "waiting for daemon to stop (PID %i)" pid)
         (fun () -> Option.some_if (Fcntl.lock_get fd Fcntl.Write = None) ()))

type mode =
  | Start
  | Stop

let modes = [ ("start", Start); ("stop", Stop) ]

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
     in
     try
       match mode with
       | Some Start -> `Ok (start ~foreground ~port_path ~root)
       | Some Stop -> `Ok (stop ~port_path)
       | None -> `Help (`Pager, Some name)
     with Failure msg -> `Error (false, msg)

let command = (term, info)
