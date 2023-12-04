open! Stdune
open! Fiber.O
module Dune_rpc = Dune_rpc_private
open Dune_rpc
open Dune_rpc_server
module Scheduler = Test_scheduler

let () = Printexc.record_backtrace false
let () = Dune_util.Log.init_disabled ()
let print pp = Format.printf "%a@." Pp.to_fmt pp
let print_dyn dyn = print (Dyn.pp dyn)

module Chan = struct
  module Mvar = Fiber.Mvar

  type t =
    { (* Read end. Populated by writing by [snd] *)
      in_ : Sexp.t Fiber.Stream.In.t * Sexp.t Fiber.Stream.Out.t
    ; (* Write end. Can be read via [fst] *)
      out : Sexp.t Fiber.Stream.In.t * Sexp.t Fiber.Stream.Out.t
    }

  let create () = { in_ = Fiber.Stream.pipe (); out = Fiber.Stream.pipe () }
  let close t = Fiber.Stream.Out.write (snd t.out) None

  let write t s =
    let+ () =
      Fiber.sequential_iter s ~f:(fun s -> Fiber.Stream.Out.write (snd t.out) (Some s))
    in
    Ok ()
  ;;

  let read t = Fiber.Stream.In.read (fst t.in_)

  let connect c1 c2 =
    Fiber.fork_and_join_unit
      (fun () -> Fiber.Stream.connect (fst c1.out) (snd c2.in_))
      (fun () -> Fiber.Stream.connect (fst c2.out) (snd c1.in_))
  ;;

  let name _ = "unnamed"
end

module Drpc = struct
  module Client =
    Dune_rpc.Client.Make
      (Dune_rpc_client.Private.Fiber)
      (struct
        include Chan

        let write t = function
          | None -> close t
          | Some packets -> write t packets >>| Result.ok_exn
        ;;
      end)

  module Server = Dune_rpc_server.Make (Chan)
end

open Drpc

let on_init _ _ = Fiber.return ()

let setup_client_server () =
  let client_chan = Chan.create () in
  let server_chan = Chan.create () in
  let sessions = Fiber.Stream.In.of_list [ server_chan ] in
  let connect () = Chan.connect client_chan server_chan in
  client_chan, sessions, connect
;;

let test ?(private_menu = []) ?(real_methods = true) ~client ~handler ~init () =
  if real_methods
  then
    Handler.implement_notification handler Procedures.Public.shutdown (fun _ _ ->
      failwith "shutdown called");
  let run =
    let client_chan, sessions, connect = setup_client_server () in
    let client () =
      Drpc.Client.connect_with_menu client_chan init ~private_menu ~f:(fun c ->
        let* () = client c in
        Chan.close client_chan)
    in
    let server () =
      let+ () = Drpc.Server.serve sessions None (Dune_rpc_server.make handler) in
      printfn "server: finished."
    in
    Fiber.parallel_iter [ connect; client; server ] ~f:(fun f -> f ())
  in
  Scheduler.run (Scheduler.create ()) run
;;

let init ?(id = Id.make (Csexp.Atom "test-client")) ?(version = 1, 1) () =
  { Initialize.Request.dune_version = version
  ; protocol_version = Protocol.latest_version
  ; id
  }
;;

let%expect_test "initialize scheduler with rpc" =
  let handler = Handler.create ~on_init ~version:(2, 0) () in
  let init = init () in
  test
    ~init
    ~client:(fun _ ->
      printfn "client: connected. now terminating";
      Fiber.return ())
    ~handler
    ();
  [%expect {|
    client: connected. now terminating
    server: finished. |}]
;;

let%expect_test "no methods in common" =
  let handler = Handler.create ~on_init ~version:(2, 0) () in
  let init = init ~version:(2, 5) () in
  test ~init ~real_methods:false ~client:(fun _ -> assert false) ~handler ();
  [%expect.unreachable]
[@@expect.uncaught_exn
  {|
  ( "Server_aborted\
   \n  [ [ \"message\"; \"Server and client have no method versions in common\" ] ]")
  Trailing output
  ---------------
  server: finished. |}]
;;

let simple_request
  (type a b)
  ?(version = 1)
  ~method_
  (req : (a, Conv.values) Conv.t)
  (resp : (b, Conv.values) Conv.t)
  =
  let v = Decl.Request.make_current_gen ~req ~resp ~version in
  Decl.Request.make ~method_ ~generations:[ v ]
;;

let request_exn client witness n =
  let* staged = Client.Versioned.prepare_request client witness in
  let staged =
    match staged with
    | Ok s -> s
    | Error e -> raise (Dune_rpc.Version_error.E e)
  in
  Client.request client staged n
;;

let%expect_test "call method with matching versions" =
  let decl = simple_request ~method_:"double" Conv.int Conv.int in
  let handler =
    let rpc = Handler.create ~on_init ~version:(1, 1) () in
    let () =
      let cb _ x =
        if x = 0
        then
          raise
            (Response.Error.E
               (Response.Error.create ~kind:Invalid_request ~message:"0 not allowed" ()))
        else Fiber.return (x + x)
      in
      Handler.implement_request rpc decl cb
    in
    rpc
  in
  let witness = Decl.Request.witness decl in
  let client client =
    printfn "client: sending request";
    let* resp = request_exn client witness 5 in
    (match resp with
     | Error _ -> assert false
     | Ok s -> printfn "client: result %d" s);
    printfn "client: sending invalid request";
    let* resp = request_exn client witness 0 in
    (match resp with
     | Error e -> printfn "client: error %s" e.message
     | Ok _ -> assert false);
    Fiber.return ()
  in
  let init =
    { Initialize.Request.dune_version = 1, 1
    ; protocol_version = Protocol.latest_version
    ; id = Id.make (Atom "test-client")
    }
  in
  test ~init ~client ~handler ~private_menu:[ Request decl ] ();
  [%expect
    {|
    client: sending request
    client: result 10
    client: sending invalid request
    client: error 0 not allowed
    server: finished. |}]
;;

let%expect_test "call method with no matching versions" =
  let decl = simple_request ~method_:"double" Conv.int Conv.int in
  let handler =
    let rpc = Handler.create ~on_init ~version:(2, 0) () in
    let () =
      let cb _ x = Fiber.return (x + x) in
      Handler.implement_request rpc decl cb
    in
    rpc
  in
  let client client =
    printfn "client: preparing request";
    let* resp = Client.Versioned.prepare_request client (Decl.Request.witness decl) in
    (match resp with
     | Error e -> printfn "client: error %s" (Dune_rpc.Version_error.message e)
     | Ok _ -> assert false);
    Fiber.return ()
  in
  let init =
    { Initialize.Request.dune_version = 1, 1
    ; protocol_version = Protocol.latest_version
    ; id = Id.make (Atom "test-client")
    }
  in
  let decl' = simple_request ~method_:"double" ~version:2 Conv.int Conv.int in
  test ~init ~client ~handler ~private_menu:[ Request decl' ] ();
  [%expect
    {|
    client: preparing request
    client: error invalid method
    server: finished. |}]
;;

module Add = struct
  type req =
    { x : int
    ; y : int
    ; others : int list
    }

  type resp =
    | No_others of int
    | With_others of
        { xy : int
        ; all : int
        }

  module V1_only = struct
    let req = Conv.pair Conv.int Conv.int
    let resp = Conv.int
  end

  let v1_only =
    Decl.Request.make_current_gen
      ~req:(Conv.pair Conv.int Conv.int)
      ~resp:Conv.int
      ~version:1
  ;;

  let v1 =
    let upgrade_req (x, y) = { x; y; others = [] } in
    let downgrade_req { x; y; others = _ } = x, y in
    let upgrade_resp x = No_others x in
    let downgrade_resp = function
      | No_others x -> x
      | With_others { xy; all = _ } -> xy
    in
    Decl.Request.make_gen
      ~req:(Conv.pair Conv.int Conv.int)
      ~resp:Conv.int
      ~upgrade_req
      ~downgrade_req
      ~upgrade_resp
      ~downgrade_resp
      ~version:1
  ;;

  let v2 =
    let req =
      let open Conv in
      let parse =
        record
          (three
             (field "x" (required int))
             (field "y" (required int))
             (field "others" (required (list int))))
      in
      let to_ (x, y, others) = { x; y; others } in
      let from { x; y; others } = x, y, others in
      iso parse to_ from
    in
    let resp =
      let open Conv in
      let no_others = constr "no_others" int (fun x -> No_others x) in
      let with_others =
        constr "with_others" (pair int int) (fun (xy, all) -> With_others { xy; all })
      in
      sum
        [ econstr no_others; econstr with_others ]
        (function
          | No_others x -> case x no_others
          | With_others { xy; all } -> case (xy, all) with_others)
    in
    Decl.Request.make_current_gen ~req ~resp ~version:2
  ;;
end

let add_v1_only = Decl.Request.make ~method_:"add" ~generations:[ Add.v1_only ]
let add_v1_v2 = Decl.Request.make ~method_:"add" ~generations:[ Add.v1; Add.v2 ]

let%expect_test "client is newer than server" =
  let handler =
    let rpc = Handler.create ~on_init ~version:(2, 0) () in
    let () =
      let cb _ (x, y) = Fiber.return (x + y) in
      Handler.implement_request rpc add_v1_only cb
    in
    rpc
  in
  let client client =
    printfn "client: sending request";
    let+ resp =
      request_exn
        client
        (Decl.Request.witness add_v1_v2)
        { x = 10; y = 15; others = [ -25 ] }
    in
    match resp with
    | Error _ -> assert false
    | Ok (With_others _) -> assert false
    | Ok (No_others x) -> printfn "client: %d" x
  in
  let init =
    { Initialize.Request.dune_version = 1, 9
    ; protocol_version = Protocol.latest_version
    ; id = Id.make (Atom "test-client")
    }
  in
  test ~private_menu:[ Request add_v1_v2 ] ~init ~client ~handler ();
  [%expect {|
    client: sending request
    client: 25
    server: finished. |}]
;;

let%expect_test "client is older than server" =
  let handler =
    let rpc = Handler.create ~on_init ~version:(2, 0) () in
    let () =
      let cb _ { Add.x; y; others } =
        match others with
        | [] -> Fiber.return (Add.No_others (x + y))
        | _ :: _ -> assert false
      in
      Handler.implement_request rpc add_v1_v2 cb
    in
    rpc
  in
  let client client =
    printfn "client: sending request";
    let+ resp = request_exn client (Decl.Request.witness add_v1_only) (20, 30) in
    match resp with
    | Error _ -> assert false
    | Ok x -> printfn "client: %d" x
  in
  let init =
    { Initialize.Request.dune_version = 1, 9
    ; protocol_version = Protocol.latest_version
    ; id = Id.make (Atom "test-client")
    }
  in
  test ~private_menu:[ Request add_v1_only ] ~init ~client ~handler ();
  [%expect {|
    client: sending request
    client: 50
    server: finished. |}]
;;

let%test_module "long polling" =
  (module struct
    let v1 =
      Decl.Request.make_current_gen ~req:Id.sexp ~resp:(Conv.option Conv.int) ~version:1
    ;;

    let sub_proc =
      Dune_rpc.Procedures.Poll.make (Dune_rpc.Procedures.Poll.Name.make "pulse") [ v1 ]
    ;;

    let sub_decl = Sub.of_procedure sub_proc
    let version = 3, 0
    let init = init ~version ()
    let rpc () = Handler.create ~on_init ~version ()

    let server f =
      let rpc = rpc () in
      let () =
        let on_poll _session = f () in
        let on_cancel _session =
          printfn "server: polling cancelled";
          Fiber.return ()
        in
        Handler.For_tests.implement_poll rpc sub_proc ~on_poll ~on_cancel
      in
      rpc
    ;;

    let server_long_poll svar =
      let rpc = rpc () in
      let () =
        Handler.implement_long_poll
          rpc
          sub_proc
          svar
          ~equal:Int.equal
          ~diff:(fun ~last ~now ->
            match last with
            | None -> now
            | Some last -> now - last)
      in
      rpc
    ;;

    let%expect_test "long polling - client side termination" =
      let client client =
        let* poller = Client.poll client sub_decl in
        let poller =
          match poller with
          | Ok p -> p
          | Error e -> raise (Version_error.E e)
        in
        let req () =
          let+ res = Client.Stream.next poller in
          match res with
          | None -> printfn "client: no more values"
          | Some a -> printfn "client: received %d" a
        in
        let* () = req () in
        let* () = req () in
        Client.Stream.cancel poller
      in
      let handler =
        let state = ref 0 in
        server (fun () ->
          incr state;
          Fiber.return (Some !state))
      in
      test ~init ~client ~handler ~private_menu:[ Poll sub_proc ] ();
      [%expect
        {|
    client: received 1
    client: received 2
    server: polling cancelled
    server: finished. |}]
    ;;

    let%expect_test "long polling - server side termination" =
      let client client =
        printfn "client: long polling";
        let* poller = Client.poll client sub_decl in
        let poller =
          match poller with
          | Ok p -> p
          | Error e -> raise (Version_error.E e)
        in
        let+ () =
          Fiber.repeat_while ~init:() ~f:(fun () ->
            let+ res = Client.Stream.next poller in
            match res with
            | None -> None
            | Some a ->
              printfn "client: received %d" a;
              Some ())
        in
        printfn "client: subscription terminated"
      in
      let handler =
        let state = ref 0 in
        server (fun _poller ->
          incr state;
          Fiber.return (if !state = 3 then None else Some !state))
      in
      test ~init ~client ~handler ~private_menu:[ Poll sub_proc ] ();
      [%expect
        {|
    client: long polling
    client: received 1
    client: received 2
    client: subscription terminated
    server: finished. |}]
    ;;

    let%expect_test "long polling - client cancels while request is in-flight" =
      let ready_to_cancel : unit Fiber.Ivar.t = Fiber.Ivar.create () in
      let svar = Fiber.Svar.create 0 in
      let handler = server_long_poll svar in
      let client client =
        let* poller =
          let+ poller = Client.poll client sub_decl in
          match poller with
          | Ok p -> p
          | Error e -> raise (Version_error.E e)
        in
        let req () =
          let+ res = Client.Stream.next poller in
          match res with
          | None -> printfn "client: no more values"
          | Some a -> printfn "client: received %d" a
        in
        let* () = Fiber.Svar.write svar 1 in
        let* () = req () in
        Fiber.fork_and_join_unit
          (fun () ->
            let* () = Fiber.Ivar.read ready_to_cancel in
            printfn "client: cancelling";
            Client.Stream.cancel poller)
          (fun () ->
            printfn "client: waiting for second value (that will never come)";
            let+ () = Fiber.fork_and_join_unit req (Fiber.Ivar.fill ready_to_cancel) in
            printfn "client: finishing session")
      in
      test ~init ~client ~handler ~private_menu:[ Poll sub_proc ] ();
      [%expect.unreachable]
    [@@expect.uncaught_exn
      {|
      (Test_scheduler.Never)
      Trailing output
      ---------------
      client: received 1
      client: waiting for second value (that will never come)
      client: cancelling
      client: no more values
      client: finishing session |}]
    ;;

    let%expect_test "long polling - server side termination" =
      let client client =
        printfn "client: long polling";
        let* poller = Client.poll client sub_decl in
        let poller =
          match poller with
          | Ok p -> p
          | Error e -> raise (Version_error.E e)
        in
        let+ () =
          Fiber.repeat_while ~init:() ~f:(fun () ->
            let+ res = Client.Stream.next poller in
            match res with
            | None -> None
            | Some a ->
              printfn "client: received %d" a;
              Some ())
        in
        printfn "client: subscription terminated"
      in
      let handler =
        let state = ref 0 in
        server (fun _poller ->
          incr state;
          Fiber.return (if !state = 3 then None else Some !state))
      in
      test ~init ~client ~handler ~private_menu:[ Poll sub_proc ] ();
      [%expect
        {|
    client: long polling
    client: received 1
    client: received 2
    client: subscription terminated
    server: finished. |}]
    ;;
  end)
;;

let%expect_test "server to client request" =
  let decl = simple_request ~method_:"double" Conv.int Conv.int in
  let client_finish = Fiber.Ivar.create () in
  let pool = Fiber.Pool.create () in
  let on_upgrade session _menu =
    let witness = Decl.Request.witness decl in
    let* () =
      Fiber.Pool.task pool ~f:(fun () ->
        let* () = Fiber.Pool.close pool in
        print_endline "server: sending request to client";
        let+ res =
          Session.request session witness (Dune_rpc.Id.make (Csexp.Atom "test")) 10
        in
        Printf.printf "client: received response %d\n" res)
    in
    Fiber.Ivar.fill client_finish ()
  in
  let handler = Handler.create ~on_init ~on_upgrade ~version:(2, 0) () in
  Handler.declare_request handler decl;
  let client _ =
    Fiber.fork_and_join_unit
      (fun () -> Fiber.Ivar.read client_finish)
      (fun () -> Fiber.Pool.run pool)
  in
  let init =
    { Initialize.Request.dune_version = 1, 1
    ; protocol_version = Protocol.latest_version
    ; id = Id.make (Atom "test-client")
    }
  in
  test
    ~init
    ~client
    ~handler
    ~private_menu:
      [ Handle_request
          ( decl
          , let doubler x =
              print_endline "client: received request from server";
              Fiber.return (x * 2)
            in
            doubler )
      ]
    ();
  [%expect
    {|
    server: sending request to client
    client: received request from server
    client: received response 20
    server: finished. |}]
;;

let%test_module "finalization" =
  (module struct
    let decl = simple_request ~method_:"double" Conv.unit Conv.unit
    let witness = Decl.Request.witness decl

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
        |       "connection terminated. this request will never receive a response"
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
        |       "connection terminated. this request will never receive a response"
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


        --------------- |}]
    ;;
  end)
;;

let%expect_test "sexp_for_digest" =
  let open Dune_rpc_private in
  let print_sexp_for_digest conv =
    Pp.to_fmt Format.std_formatter (Sexp.pp (Conv.sexp_for_digest conv))
  in
  print_sexp_for_digest
    (Conv.five
       (Conv.field "a" (Conv.required Conv.string))
       (Conv.field "b" (Conv.optional Conv.int))
       (Conv.field "c" (Conv.required Conv.float))
       (Conv.field "d" (Conv.required Conv.unit))
       (Conv.field "e" (Conv.optional Conv.char)));
  [%expect
    {|
      (Iso
       (Both
        (Both (Field a (Required String)) (Field b (Optional Int)))
        (Iso
         (Both
          (Field c (Required Float))
          (Both (Field d (Required Unit)) (Field e (Optional Char))))))) |}];
  print_sexp_for_digest (Conv.iso Conv.sexp (fun x -> x) (fun x -> x));
  [%expect {| (Iso Sexp) |}];
  let id_iso = Conv.iso Conv.sexp (fun x -> x) (fun x -> x) in
  print_sexp_for_digest
    (Conv.pair
       (Conv.version ~until:(1, 2) ~since:(2, 3) id_iso)
       (Conv.version ~since:(1, 2) id_iso));
  [%expect
    {|
    (Pair
     (Version (Iso Sexp) (since 2 3) (until 1 2))
     (Version (Iso Sexp) (since 1 2))) |}];
  let list_conv inner =
    Conv.fixpoint (fun conv ->
      let nil = Conv.constr "nil" Conv.unit (fun () -> []) in
      let cons = Conv.constr "cons" (Conv.pair inner conv) (fun (x, xs) -> x :: xs) in
      Conv.sum
        [ Conv.econstr nil; Conv.econstr cons ]
        (function
          | [] -> Conv.case () nil
          | x :: xs -> Conv.case (x, xs) cons))
  in
  print_sexp_for_digest (list_conv Conv.int);
  [%expect {| (Fixpoint (Sum (nil Unit) (cons (Pair Int (Recurse 0))))) |}];
  print_sexp_for_digest (list_conv (list_conv Conv.int));
  (* Recursion uses De Bruijn indices because we want equal structures to
     produce the same digest. *)
  [%expect
    {|
    (Fixpoint
     (Sum
      (nil Unit)
      (cons
       (Pair
        (Fixpoint (Sum (nil Unit) (cons (Pair Int (Recurse 0)))))
        (Recurse 0))))) |}]
;;

let%expect_test "print digests for all public RPCs" =
  let open Dune_rpc_private in
  Decl.Request.print_generations Procedures.Public.ping;
  [%expect {|
    Version 1:
      Request: Unit
      Response: Unit
    |}];
  Decl.Request.print_generations Procedures.Public.diagnostics;
  [%expect
    {|
    Version 1:
      Request: Unit
      Response: ffd3de9652c685594aacfc51d28f2533
    Version 2:
      Request: Unit
      Response: 0d4442e0c36d6727a9acf9aabce6a6ad
    |}];
  Decl.Notification.print_generations Procedures.Public.shutdown;
  [%expect {| Version 1: Unit |}];
  Decl.Request.print_generations Procedures.Public.format_dune_file;
  [%expect
    {|
    Version 1:
      Request: 15eae4b546faf05a0fc3b6d03aed0c63
      Response: String
    |}];
  Decl.Request.print_generations Procedures.Public.promote;
  [%expect {|
    Version 1:
      Request: String
      Response: Unit
    |}];
  Decl.Request.print_generations Procedures.Public.build_dir;
  [%expect {|
    Version 1:
      Request: Unit
      Response: String
    |}];
  Decl.Notification.print_generations Procedures.Server_side.abort;
  [%expect {| Version 1: 0e9dfd1099101769896cf0bb06f891c6 |}];
  Decl.Notification.print_generations Procedures.Server_side.log;
  [%expect {| Version 1: 0e9dfd1099101769896cf0bb06f891c6 |}];
  Decl.Request.print_generations (Procedures.Poll.poll Procedures.Poll.progress);
  [%expect
    {|
    Version 1:
      Request: Sexp
      Response: 889aa68f4ad3fc68ef5dfffbb7282c18
    Version 2:
      Request: Sexp
      Response: 929074caab98360dc7116b6f27c2b9ad
    |}];
  Decl.Request.print_generations (Procedures.Poll.poll Procedures.Poll.diagnostic);
  [%expect
    {|
    Version 1:
      Request: Sexp
      Response: 443627a52ab5595206164d020ff01c56
    Version 2:
      Request: Sexp
      Response: 12995aa06697c01ef35c0339bd2fa29e
    |}];
  Decl.Request.print_generations (Procedures.Poll.poll Procedures.Poll.running_jobs);
  [%expect
    {|
    Version 1:
      Request: Sexp
      Response: 33528f248084297d123a6ebd4c3ddee0
    |}]
;;
