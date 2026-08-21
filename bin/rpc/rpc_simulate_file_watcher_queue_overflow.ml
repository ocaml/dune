open Import

let info =
  let doc = "Simulate a file-watcher queue overflow." in
  Cmd.info "simulate-file-watcher-queue-overflow" ~doc
;;

let term =
  let+ (builder : Common.Builder.t) = Common.Builder.term
  and+ wait = Rpc_common.wait_term in
  Rpc_common.client_term builder
  @@ fun () ->
  let open Fiber.O in
  let+ response =
    Rpc_common.fire_request
      ~name:"simulate_file_watcher_queue_overflow_cmd"
      ~wait
      builder
      Dune_rpc_impl.Decl.simulate_file_watcher_queue_overflow
      ()
  in
  let open Dune_rpc_impl.Decl.Queue_overflow in
  match response with
  | Ok -> ()
  | Not_in_watch_mode ->
    User_error.raise
      [ Pp.text "queue overflow simulation is only available in watch mode" ]
  | Build_failed ->
    User_error.raise
      [ Pp.text "the build triggered by the simulated queue overflow failed" ]
;;

let cmd = Cmd.v info term
