open Import
open Scheduler

let on_event = function
  | Run.Event.Build_interrupted ->
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
  let rpc_server =
    match Common.rpc common with
    | `Allow server -> server
    | `Forbid_builds -> Code_error.raise "rpc must be enabled in polling mode" []
  in
  let server = rpc rpc_server in
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
  Run.go config ~file_watcher ~on_event run
;;
