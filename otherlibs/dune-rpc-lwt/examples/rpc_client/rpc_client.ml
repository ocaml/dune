let () =
  let init =
    Dune_rpc.V1.Initialize.create
      ~id:(Dune_rpc.V1.Id.make (Csexp.Atom "example_rpc_client"))
  in
  let where = Dune_rpc_lwt.V1.Where.default ~build_dir:"_build" () in
  Lwt_main.run
    (let open Lwt.Syntax in
     let open Lwt.Infix in
     let* chan = Dune_rpc_lwt.V1.connect_chan where in
     Dune_rpc_lwt.V1.Client.connect chan init ~f:(fun client ->
       print_endline "Sending ping to server...";
       let* () =
         let* request =
           Dune_rpc_lwt.V1.Client.Versioned.prepare_request
             client
             Dune_rpc.V1.Request.ping
           >|= Result.get_ok
         in
         Dune_rpc_lwt.V1.Client.request client request () >|= Result.get_ok
       in
       print_endline "Got response from server...";
       print_endline "Creating progress stream...";
       let* progress_stream =
         Dune_rpc_lwt.V1.Client.poll client Dune_rpc.V1.Sub.progress >|= Result.get_ok
       in
       print_endline "Waiting for next progress event...";
       let* progress_event = Dune_rpc_lwt.V1.Client.Stream.next progress_stream in
       let message =
         match progress_event with
         | None -> "(none)"
         | Some Success -> "Success"
         | Some Failed -> "Failed"
         | Some Interrupted -> "Interrupted"
         | Some (In_progress { complete; remaining; failed }) ->
           Printf.sprintf
             "In_progress { complete = %d;  remaining = %d;  failed = %d }"
             complete
             remaining
             failed
         | Some Waiting -> "Waiting"
       in
       print_endline (Printf.sprintf "Got progress_event: %s" message);
       print_endline "Shutting down RPC server...";
       let* () =
         let* shutdown_notification =
           Dune_rpc_lwt.V1.Client.Versioned.prepare_notification
             client
             Dune_rpc.V1.Notification.shutdown
           >|= Result.get_ok
         in
         Dune_rpc_lwt.V1.Client.notification client shutdown_notification ()
       in
       Lwt.return ()))
;;
