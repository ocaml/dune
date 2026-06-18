open Import
open Scheduler

let no_build_no_rpc ~config:dune_config f =
  let config =
    Dune_config.for_scheduler dune_config ~print_ctrl_c_warning:true ~watch_exclusions:[]
  in
  Dune_rules.Clflags.concurrency := config.concurrency;
  Run.go config f
;;

let go_without_rpc_server ~(common : Common.t) ~config:dune_config f =
  let config =
    let watch_exclusions = Common.watch_exclusions common in
    Dune_config.for_scheduler dune_config ~print_ctrl_c_warning:true ~watch_exclusions
  in
  Dune_rules.Clflags.concurrency := config.concurrency;
  Run.go config f
;;

let await_action_runner common =
  if Common.action_runner_requested common
  then
    let open Fiber.O in
    (* [Common.action_runner] spawns a worker that immediately connects back to
       Dune over RPC. With eager socket creation, the socket can be connectable
       before the accept loop is running, so start the server before spawning
       the worker. *)
    let* () = Dune_rpc_impl.Server.ensure_ready () in
    match Common.action_runner common with
    | None -> Fiber.return ()
    | Some action_runner -> Dune_engine.Action_runner.ensure_ready action_runner
  else Fiber.return ()
;;

let go_with_rpc_server ~common ~config f =
  let f =
    match Common.rpc common with
    | `Allow server ->
      fun () ->
        Dune_rpc_impl.Server.with_background_rpc server (fun () ->
          let open Fiber.O in
          let* () = await_action_runner common in
          f ())
    | `Forbid_builds -> f
  in
  go_without_rpc_server ~common ~config f
;;

let go_with_rpc_server_and_file_watcher ~(common : Common.t) ~config:dune_config run =
  let config =
    let watch_exclusions = Common.watch_exclusions common in
    Dune_config.for_scheduler dune_config ~print_ctrl_c_warning:true ~watch_exclusions
  in
  Dune_rules.Clflags.concurrency := config.concurrency;
  let file_watcher = Common.file_watcher common in
  let run () =
    match Common.rpc common with
    | `Allow server ->
      Dune_rpc_impl.Server.with_background_rpc server
      @@ fun () ->
      let open Fiber.O in
      let* () = Dune_rpc_impl.Server.ensure_ready () in
      let* () = await_action_runner common in
      run ()
    | `Forbid_builds -> run ()
  in
  Run.go config ~file_watcher run
;;
