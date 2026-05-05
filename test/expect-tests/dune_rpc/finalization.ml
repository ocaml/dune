open Stdune
open Fiber.O
module Dune_rpc = Dune_rpc.Private
open Dune_rpc
open Rpc.Server
open Common

let print pp = Format.printf "%a@." Pp.to_fmt pp
let print_dyn dyn = print (Dyn.pp dyn)
let decl = simple_request ~method_:(Method.Name.of_string "double") Conv.unit Conv.unit
let witness = Decl.Request.witness decl

let server_request_decl =
  simple_request ~method_:(Method.Name.of_string "server-request") Conv.unit Conv.unit
;;

let () = Printexc.record_backtrace false

type callback =
  | Print
  | Fail

let dyn_of_callback =
  let open Dyn in
  function
  | Print -> variant "Print" []
  | Fail -> variant "Fail" []
;;

type callbacks =
  { on_init : callback
  ; on_terminate : callback
  ; on_upgrade : callback
  }

let dyn_of_callback { on_init; on_terminate; on_upgrade } =
  Dyn.record
    [ "on_init", dyn_of_callback on_init
    ; "on_terminate", dyn_of_callback on_terminate
    ; "on_upgrade", dyn_of_callback on_upgrade
    ]
;;

let handler { on_init; on_terminate; on_upgrade } =
  let f name what =
    printfn "server: %s" name;
    match what with
    | Print -> Fiber.return ()
    | Fail -> raise Dune_util.Report_error.Already_reported
  in
  let on_init _ _ = f "init" on_init in
  let on_terminate _ = f "terminate" on_terminate in
  let on_upgrade _ _ = f "upgrade" on_upgrade in
  Handler.create ~on_terminate ~on_init ~on_upgrade ~version:(1, 1) ()
;;

let test callback =
  let handler =
    let rpc = handler callback in
    let () =
      let cb _ () = failwith "never works" in
      Handler.implement_request rpc decl cb
    in
    rpc
  in
  let client client =
    printfn "client: sending request";
    let+ resp = request_exn client witness () in
    match resp with
    | Error error -> print_dyn @@ Response.Error.to_dyn error
    | Ok _ -> assert false
  in
  let init =
    { Initialize.Request.dune_version = 1, 1
    ; protocol_version = Protocol.latest_version
    ; id = Id.make (Atom "test-client")
    }
  in
  test ~init ~client ~handler ~private_menu:[ Request decl ] ()
;;

let%expect_test "termination is always called" =
  let kind = [ Print; Fail ] in
  let callbacks =
    List.concat_map kind ~f:(fun on_init ->
      List.concat_map kind ~f:(fun on_terminate ->
        List.concat_map kind ~f:(fun on_upgrade ->
          [ { on_init; on_terminate; on_upgrade } ])))
  in
  List.iter callbacks ~f:(fun callback ->
    dyn_of_callback callback |> print_dyn;
    (try test callback with
     | exn ->
       let exn = Exn_with_backtrace.capture exn in
       Format.printf "%a@.@." Exn_with_backtrace.pp_uncaught exn);
    print_endline "---------------");
  [%expect
    {|
    { on_init = Print; on_terminate = Print; on_upgrade = Print }
    server: init
    server: upgrade
    client: sending request
    { payload =
        Some [ [ [ "exn"; "Failure(\"never works\")" ]; [ "backtrace"; "" ] ] ]
    ; message = "server error"
    ; kind = Code_error
    }
    server: terminate
    server: finished.
    ---------------
    { on_init = Print; on_terminate = Print; on_upgrade = Fail }
    server: init
    server: upgrade
    server: terminate
    server: finished.
    client: sending request
    { payload =
        Some
          [ [ "id"; [ "auto"; "0" ] ]
          ; [ "req"; [ [ "method"; "double" ]; [ "params"; [] ] ] ]
          ]
    ; message = "request sent while connection is dead"
    ; kind = Connection_dead
    }
    ---------------
    { on_init = Print; on_terminate = Fail; on_upgrade = Print }
    server: init
    server: upgrade
    client: sending request
    { payload =
        Some [ [ [ "exn"; "Failure(\"never works\")" ]; [ "backtrace"; "" ] ] ]
    ; message = "server error"
    ; kind = Code_error
    }
    server: terminate
    server: finished.
    ---------------
    { on_init = Print; on_terminate = Fail; on_upgrade = Fail }
    server: init
    server: upgrade
    server: terminate
    /-----------------------------------------------------------------------
    | Internal error: Uncaught exception.
    | Dune_util__Report_error.Already_reported
    \-----------------------------------------------------------------------


    ---------------
    { on_init = Fail; on_terminate = Print; on_upgrade = Print }
    server: init
    server: terminate
    server: finished.
    /-----------------------------------------------------------------------
    | Internal error: Uncaught exception.
    | Response.E
    |   { payload = Some [ [ "id"; [ "initialize" ] ] ]
    |   ; message =
    |       "Connection terminated. This request will never receive a response."
    |   ; kind = Connection_dead
    |   }
    \-----------------------------------------------------------------------


    ---------------
    { on_init = Fail; on_terminate = Print; on_upgrade = Fail }
    server: init
    server: terminate
    server: finished.
    /-----------------------------------------------------------------------
    | Internal error: Uncaught exception.
    | Response.E
    |   { payload = Some [ [ "id"; [ "initialize" ] ] ]
    |   ; message =
    |       "Connection terminated. This request will never receive a response."
    |   ; kind = Connection_dead
    |   }
    \-----------------------------------------------------------------------


    ---------------
    { on_init = Fail; on_terminate = Fail; on_upgrade = Print }
    server: init
    server: terminate
    /-----------------------------------------------------------------------
    | Internal error: Uncaught exception.
    | Dune_util__Report_error.Already_reported
    \-----------------------------------------------------------------------


    ---------------
    { on_init = Fail; on_terminate = Fail; on_upgrade = Fail }
    server: init
    server: terminate
    /-----------------------------------------------------------------------
    | Internal error: Uncaught exception.
    | Dune_util__Report_error.Already_reported
    \-----------------------------------------------------------------------


    ---------------
    |}]
;;

let%expect_test "server-side pending request fails on client disconnect" =
  let module Client = struct
    include
      Dune_rpc.Client.Make
        (Rpc.Private.Fiber)
        (struct
          include Chan

          let write t packets =
            write t packets
            >>| function
            | Ok () -> ()
            | Error `Closed ->
              (* Keep this test focused on the server-side pending request path.
                 The default test client raises [Already_reported] here instead. *)
              ()
          ;;
        end)
  end
  in
  let upgraded_session = Fiber.Ivar.create () in
  let client_request_started = Fiber.Ivar.create () in
  let release_client_request = Fiber.Ivar.create () in
  let handler =
    let on_upgrade session _menu = Fiber.Ivar.fill upgraded_session session in
    let rpc = Handler.create ~on_init ~on_upgrade ~version:(1, 1) () in
    let () = Handler.declare_request rpc server_request_decl in
    rpc
  in
  let init =
    { Initialize.Request.dune_version = 1, 1
    ; protocol_version = Protocol.latest_version
    ; id = Id.make (Atom "test-client")
    }
  in
  let run =
    let client_chan, sessions = setup_direct_client_server () in
    let client () =
      let private_menu =
        [ Client.Handle_request
            ( server_request_decl
            , fun () ->
                let* () = Fiber.Ivar.fill client_request_started () in
                Fiber.Ivar.read release_client_request )
        ]
      in
      Client.connect_with_menu client_chan init ~private_menu ~f:(fun _client ->
        let* () = Fiber.Ivar.read client_request_started in
        let* () =
          printfn "client: closing channel";
          Chan.close client_chan
        in
        Fiber.Ivar.fill release_client_request ())
    in
    let requester () =
      let* session = Fiber.Ivar.read upgraded_session in
      let id = Id.make (Atom "server-request") in
      let* result =
        Fiber.collect_errors (fun () ->
          Session.request session (Decl.Request.witness server_request_decl) id ())
      in
      match result with
      | Ok () -> Code_error.raise "server request unexpectedly completed" []
      | Error exns ->
        printfn "server: request failed";
        print_dyn (Dyn.list Exn_with_backtrace.to_dyn exns);
        Fiber.return ()
    in
    let server () =
      let+ () = Drpc.Server.serve sessions (Rpc.Server.make handler) in
      printfn "server: finished."
    in
    Fiber.parallel_iter [ client; requester; server ] ~f:(fun f -> f ())
  in
  Scheduler.run (Scheduler.create ()) run;
  [%expect
    {|
    client: closing channel
    server: finished.
    server: request failed
    [ { exn =
          "Response.E\n\
          \  { payload = Some [ [ \"id\"; \"server-request\" ] ]\n\
          \  ; message =\n\
          \      \"Connection terminated. This request will never receive a response.\"\n\
          \  ; kind = Connection_dead\n\
          \  }"
      ; backtrace = ""
      }
    ]
    |}]
;;

let%expect_test "server-side pending request keeps a response read before close" =
  let server_request_id = Id.make (Atom "server-request") in
  let upgraded_session = Fiber.Ivar.create () in
  let response_written = Fiber.Ivar.create () in
  let module Client = struct
    include
      Dune_rpc.Client.Make
        (Rpc.Private.Fiber)
        (struct
          include Chan

          let is_server_request_response sexp =
            match Conv.of_sexp ~version:(1, 1) Packet.sexp sexp with
            | Ok (Response (id, _)) -> Id.equal id server_request_id
            | Ok _ | Error _ -> false
          ;;

          let write t packets =
            let close_after_write = List.exists packets ~f:is_server_request_response in
            let* () =
              Chan.write t packets
              >>| function
              | Ok () -> ()
              | Error `Closed -> ()
            in
            if close_after_write
            then (
              printfn "client: closing channel";
              let* () = Chan.close t in
              Fiber.Ivar.fill response_written ())
            else Fiber.return ()
          ;;
        end)
  end
  in
  let handler =
    let on_upgrade session _menu = Fiber.Ivar.fill upgraded_session session in
    let rpc = Handler.create ~on_init ~on_upgrade ~version:(1, 1) () in
    let () = Handler.declare_request rpc server_request_decl in
    rpc
  in
  let init =
    { Initialize.Request.dune_version = 1, 1
    ; protocol_version = Protocol.latest_version
    ; id = Id.make (Atom "test-client")
    }
  in
  let run =
    let client_chan, sessions = setup_direct_client_server () in
    let client () =
      let private_menu =
        [ Client.Handle_request (server_request_decl, fun () -> Fiber.return ()) ]
      in
      Client.connect_with_menu client_chan init ~private_menu ~f:(fun _client ->
        Fiber.Ivar.read response_written)
    in
    let requester () =
      let* session = Fiber.Ivar.read upgraded_session in
      let* result =
        Fiber.collect_errors (fun () ->
          Session.request
            session
            (Decl.Request.witness server_request_decl)
            server_request_id
            ())
      in
      match result with
      | Ok () ->
        printfn "server: request completed";
        Fiber.return ()
      | Error exns ->
        printfn "server: request failed";
        print_dyn (Dyn.list Exn_with_backtrace.to_dyn exns);
        Fiber.return ()
    in
    let server () =
      let+ () = Drpc.Server.serve sessions (Rpc.Server.make handler) in
      printfn "server: finished."
    in
    Fiber.parallel_iter [ client; requester; server ] ~f:(fun f -> f ())
  in
  Scheduler.run (Scheduler.create ()) run;
  [%expect
    {|
    client: closing channel
    server: request completed
    server: finished.
    |}]
;;

let%expect_test "client-side reply write raises on disconnect" =
  let upgraded_session = Fiber.Ivar.create () in
  let client_request_started = Fiber.Ivar.create () in
  let release_client_request = Fiber.Ivar.create () in
  let handler =
    let on_upgrade session _menu = Fiber.Ivar.fill upgraded_session session in
    let rpc = Handler.create ~on_init ~on_upgrade ~version:(1, 1) () in
    let () = Handler.declare_request rpc server_request_decl in
    rpc
  in
  let init =
    { Initialize.Request.dune_version = 1, 1
    ; protocol_version = Protocol.latest_version
    ; id = Id.make (Atom "test-client")
    }
  in
  let run =
    let client_chan, sessions = setup_direct_client_server () in
    let client () =
      let private_menu =
        [ Drpc.Client.Handle_request
            ( server_request_decl
            , fun () ->
                let* () = Fiber.Ivar.fill client_request_started () in
                Fiber.Ivar.read release_client_request )
        ]
      in
      Drpc.Client.connect_with_menu client_chan init ~private_menu ~f:(fun _client ->
        let* () = Fiber.Ivar.read client_request_started in
        let* () =
          printfn "client: closing channel";
          Chan.close client_chan
        in
        let* () = Fiber.Ivar.fill release_client_request () in
        Fiber.return ())
    in
    let requester () =
      let* session = Fiber.Ivar.read upgraded_session in
      let id = Id.make (Atom "server-request") in
      let* result =
        Fiber.collect_errors (fun () ->
          Session.request session (Decl.Request.witness server_request_decl) id ())
      in
      match result with
      | Ok () -> Code_error.raise "server request unexpectedly completed" []
      | Error exns ->
        printfn "server: request failed";
        print_dyn (Dyn.list Exn_with_backtrace.to_dyn exns);
        Fiber.return ()
    in
    let server () =
      let+ () = Drpc.Server.serve sessions (Rpc.Server.make handler) in
      printfn "server: finished."
    in
    Fiber.parallel_iter [ client; requester; server ] ~f:(fun f -> f ())
  in
  Scheduler.run (Scheduler.create ()) run;
  [%expect
    {|
    client: closing channel
    server: finished.
    server: request failed
    [ { exn =
          "Response.E\n\
          \  { payload = Some [ [ \"id\"; \"server-request\" ] ]\n\
          \  ; message =\n\
          \      \"Connection terminated. This request will never receive a response.\"\n\
          \  ; kind = Connection_dead\n\
          \  }"
      ; backtrace = ""
      }
    ]
    |}]
;;

let%expect_test "client-side request write raises after close" =
  let handler =
    let rpc = Handler.create ~on_init ~version:(1, 1) () in
    let () = Handler.implement_request rpc decl (fun _ () -> Fiber.return ()) in
    rpc
  in
  let init =
    { Initialize.Request.dune_version = 1, 1
    ; protocol_version = Protocol.latest_version
    ; id = Id.make (Atom "test-client")
    }
  in
  let run =
    let client_chan, sessions = setup_direct_client_server () in
    let client () =
      Drpc.Client.connect_with_menu
        client_chan
        init
        ~private_menu:[ Request decl ]
        ~f:(fun client ->
          let* () =
            printfn "client: closing channel";
            Chan.close client_chan
          in
          printfn "client: sending request";
          request_exn client witness ()
          >>| function
          | Error error -> print_dyn @@ Response.Error.to_dyn error
          | Ok _ -> assert false)
    in
    let server () =
      let+ () = Drpc.Server.serve sessions (Rpc.Server.make handler) in
      printfn "server: finished."
    in
    Fiber.parallel_iter [ client; server ] ~f:(fun f -> f ())
  in
  Scheduler.run (Scheduler.create ()) run;
  [%expect.unreachable]
[@@expect.uncaught_exn
  {|
  (Dune_util__Report_error.Already_reported)
  Trailing output
  ---------------
  client: closing channel
  client: sending request
  |}]
;;
