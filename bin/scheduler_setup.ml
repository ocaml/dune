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

let on_event ~terminal_persistence = function
  | Run.Event.Tick -> Console.Status_line.refresh ()
  | Source_files_changed { details_hum } ->
    maybe_clear_screen ~terminal_persistence ~details_hum
  | Build_interrupted ->
    Console.Status_line.set
      (Live
         (fun () ->
           let progression =
             match Fiber.Svar.read Build_system.state with
             | Initializing
             | Restarting_current_build
             | Build_succeeded__now_waiting_for_changes
             | Build_failed__now_waiting_for_changes -> Build_system.Progress.init
             | Building progress -> progress
           in
           Pp.seq
             (Pp.tag User_message.Style.Error (Pp.verbatim "Source files changed"))
             (Pp.verbatim
                (sprintf
                   ", restarting current build... (%u/%u)"
                   progression.number_of_rules_executed
                   progression.number_of_rules_discovered))))
  | Build_finish build_result ->
    let message =
      match build_result with
      | Success -> Pp.tag User_message.Style.Success (Pp.verbatim "Success")
      | Failure ->
        let failure_message =
          match
            Build_system_error.(
              Id.Map.cardinal (Set.current (Fiber.Svar.read Build_system.errors)))
          with
          | 1 -> Pp.textf "Had 1 error"
          | n -> Pp.textf "Had %d errors" n
        in
        Pp.tag User_message.Style.Error failure_message
    in
    Console.Status_line.set
      (Constant (Pp.seq message (Pp.verbatim ", waiting for filesystem changes...")))
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

let go_without_rpc_server ~(common : Common.t) ~config:dune_config f =
  let config =
    let watch_exclusions = Common.watch_exclusions common in
    Dune_config.for_scheduler dune_config ~print_ctrl_c_warning:true ~watch_exclusions
  in
  Dune_rules.Clflags.concurrency := config.concurrency;
  let on_event = on_event ~terminal_persistence:dune_config.terminal_persistence in
  Run.go config ~on_event f
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
  Run.go
    config
    ~file_watcher
    ~on_event:(on_event ~terminal_persistence:dune_config.terminal_persistence)
    run
;;
