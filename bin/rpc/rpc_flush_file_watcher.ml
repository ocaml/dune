open Import

let info =
  let doc = "Flush pending file watcher notifications in a running watch server." in
  Cmd.info "flush-file-watcher" ~doc
;;

let term =
  let+ (builder : Common.Builder.t) = Common.Builder.term
  and+ wait = Rpc_common.wait_term in
  Rpc_common.client_term builder
  @@ fun () ->
  let open Fiber.O in
  let+ response =
    Rpc_common.fire_request
      ~name:"flush_file_watcher_cmd"
      ~wait
      builder
      Dune_rpc.Procedures.Public.flush_file_watcher
      ()
  in
  match response with
  | `Ok -> ()
  | `Not_in_watch_mode ->
    User_error.raise [ Pp.text "flush-file-watcher is only available in watch mode" ]
;;

let cmd = Cmd.v info term
