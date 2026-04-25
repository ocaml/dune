open Import
open Scheduler

let maybe_clear_screen
      ~(terminal_persistence : Dune_config.Terminal_persistence.t)
      ~details_hum
  =
  match Execution_env.inside_dune with
  | true -> (* Don't print anything here to make tests less verbose *) ()
  | false ->
    (match terminal_persistence with
     | Clear_on_rebuild -> Console.reset ()
     | Clear_on_rebuild_and_flush_history -> Console.reset_flush_history ()
     | Preserve ->
       let message =
         sprintf
           "********** NEW BUILD (%s) **********"
           (String.concat ~sep:", " details_hum)
       in
       Console.print_user_message
         (User_message.make
            [ Pp.nop; Pp.tag User_message.Style.Success (Pp.verbatim message); Pp.nop ]))
;;

let set_overlay overlay message =
  Option.iter !overlay ~f:Console.Status_line.remove_overlay;
  overlay := Some (Console.Status_line.add_overlay (Constant message))
;;

let clear_overlay overlay =
  Option.iter !overlay ~f:Console.Status_line.remove_overlay;
  overlay := None
;;

let on_event overlay ~terminal_persistence = function
  | Run.Event.Tick -> Console.Status_line.refresh ()
  | Source_files_changed { details_hum } ->
    maybe_clear_screen ~terminal_persistence ~details_hum;
    clear_overlay overlay
  | Build_interrupted ->
    set_overlay
      overlay
      (Pp.tag User_message.Style.Error (Pp.verbatim "Restarting current build..."))
  | Build_finish build_result ->
    let message =
      match build_result with
      | Success ->
        Pp.seq
          (Pp.tag User_message.Style.Success (Pp.verbatim "Success"))
          (Pp.verbatim ", waiting for filesystem changes...")
      | Failure ->
        let error_count =
          Build_system_error.(
            Id.Map.cardinal (Set.current (Fiber.Svar.read Build_system.errors)))
        in
        let failure_message =
          match error_count with
          | 1 -> Pp.textf "Had 1 error"
          | n -> Pp.textf "Had %d errors" n
        in
        Pp.seq
          (Pp.tag User_message.Style.Error failure_message)
          (Pp.verbatim ", waiting for filesystem changes...")
    in
    set_overlay overlay message
;;

let rpc server =
  { Root.Rpc.Global.run = Dune_rpc_impl.Server.run server
  ; stop = Dune_rpc_impl.Server.stop server
  ; ready = Dune_rpc_impl.Server.ready server
  }
;;

let no_build_no_rpc ~config:dune_config f =
  let config =
    Dune_config.for_scheduler dune_config ~print_ctrl_c_warning:true ~watch_exclusions:[]
  in
  Dune_rules.Clflags.concurrency := config.concurrency;
  Run.go config ~on_event:(fun _ -> ()) f
;;

let run_with_watch_status ~terminal_persistence f =
  let overlay = ref None in
  Exn.protect
    ~f:(fun () ->
      let on_event = on_event overlay ~terminal_persistence in
      f on_event)
    ~finally:(fun () -> clear_overlay overlay)
;;

let go_without_rpc_server ~(common : Common.t) ~config:dune_config f =
  let config =
    let watch_exclusions = Common.watch_exclusions common in
    Dune_config.for_scheduler dune_config ~print_ctrl_c_warning:true ~watch_exclusions
  in
  Dune_rules.Clflags.concurrency := config.concurrency;
  match Common.watch common with
  | No -> Run.go config ~on_event:(fun _ -> ()) f
  | Yes _ ->
    run_with_watch_status
      ~terminal_persistence:dune_config.terminal_persistence
      (fun on_event -> Run.go config ~on_event f)
;;

let go_with_rpc_server ~common ~config f =
  let f =
    match Common.rpc common with
    | `Allow server -> fun () -> Root.Rpc.Global.with_background_rpc (rpc server) f
    | `Forbid_builds -> f
  in
  go_without_rpc_server ~common ~config f
;;

let go_with_rpc_server_and_console_status_reporting
      ~(common : Common.t)
      ~config:dune_config
      run
  =
  let server =
    match Common.rpc common with
    | `Allow server -> rpc server
    | `Forbid_builds -> Code_error.raise "rpc must be enabled in polling mode" []
  in
  let config =
    let watch_exclusions = Common.watch_exclusions common in
    Dune_config.for_scheduler dune_config ~print_ctrl_c_warning:true ~watch_exclusions
  in
  Dune_rules.Clflags.concurrency := config.concurrency;
  let file_watcher = Common.file_watcher common in
  let run () =
    let open Fiber.O in
    Root.Rpc.Global.with_background_rpc server
    @@ fun () ->
    let* () = Root.Rpc.Global.ensure_ready () in
    run ()
  in
  run_with_watch_status
    ~terminal_persistence:dune_config.terminal_persistence
    (fun on_event -> Run.go config ~file_watcher ~on_event run)
;;
