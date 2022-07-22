open! Stdune
open! Fiber.O
module Dune_rpc = Dune_rpc_private
open Dune_rpc
open Dune_rpc_server
module Scheduler = Test_scheduler

let () = Printexc.record_backtrace false

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

  let write t s =
    match s with
    | None -> Fiber.Stream.Out.write (snd t.out) None
    | Some s ->
      Fiber.sequential_iter s ~f:(fun s ->
          Fiber.Stream.Out.write (snd t.out) (Some s))

  let read t = Fiber.Stream.In.read (fst t.in_)

  let connect c1 c2 =
    Fiber.fork_and_join_unit
      (fun () -> Fiber.Stream.connect (fst c1.out) (snd c2.in_))
      (fun () -> Fiber.Stream.connect (fst c2.out) (snd c1.in_))
end

module Drpc = struct
  module Client = Dune_rpc.Client.Make (Dune_rpc_impl.Private.Fiber) (Chan)
  module Server = Dune_rpc_server.Make (Chan)
end

open Drpc

let on_init _ _ = Fiber.return ()

let setup_client_server () =
  let client_chan = Chan.create () in
  let server_chan = Chan.create () in
  let sessions = Fiber.Stream.In.of_list [ server_chan ] in
  let connect () = Chan.connect client_chan server_chan in
  (client_chan, sessions, connect)

let test ?(private_menu = []) ?(real_methods = true) ~client ~handler ~init () =
  if real_methods then
    Handler.implement_notification handler Procedures.Public.shutdown
      (fun _ _ -> failwith "shutdown called");
  let run =
    let client_chan, sessions, connect = setup_client_server () in
    let client () =
      Drpc.Client.connect_with_menu client_chan init ~private_menu ~f:(fun c ->
          let* () = client c in
          Chan.write client_chan None)
    in
    let server () =
      let+ () =
        Drpc.Server.serve sessions None (Dune_rpc_server.make handler)
      in
      printfn "server: finished."
    in
    Fiber.parallel_iter [ connect; client; server ] ~f:(fun f -> f ())
  in
  Scheduler.run (Scheduler.create ()) run

let init ?(id = Id.make (Csexp.Atom "test-client")) ?(version = (1, 1)) () =
  { Initialize.Request.dune_version = version
  ; protocol_version = Protocol.latest_version
  ; id
  }

let%expect_test "initialize scheduler with rpc" =
  let handler = Handler.create ~on_init ~version:(2, 0) () in
  let init = init () in
  test ~init
    ~client:(fun _ ->
      printfn "client: connected. now terminating";
      Fiber.return ())
    ~handler ();
  [%expect {|
    client: connected. now terminating
    server: finished. |}]

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

let simple_request (type a b) ?(version = 1) ~method_
    (req : (a, Conv.values) Conv.t) (resp : (b, Conv.values) Conv.t) =
  let v = Decl.Request.make_current_gen ~req ~resp ~version in
  Decl.Request.make ~method_ ~generations:[ v ]

let request_exn client witness n =
  let* staged = Client.Versioned.prepare_request client witness in
  let staged =
    match staged with
    | Ok s -> s
    | Error e -> raise (Dune_rpc.Version_error.E e)
  in
  Client.request client staged n

let%expect_test "call method with matching versions" =
  let decl = simple_request ~method_:"double" Conv.int Conv.int in
  let handler =
    let rpc = Handler.create ~on_init ~version:(1, 1) () in
    let () =
      let cb _ x =
        if x = 0 then
          raise
            (Response.Error.E
               (Response.Error.create ~kind:Invalid_request
                  ~message:"0 not allowed" ()))
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
    { Initialize.Request.dune_version = (1, 1)
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
    let* resp =
      Client.Versioned.prepare_request client (Decl.Request.witness decl)
    in
    (match resp with
    | Error e -> printfn "client: error %s" (Dune_rpc.Version_error.message e)
    | Ok _ -> assert false);
    Fiber.return ()
  in
  let init =
    { Initialize.Request.dune_version = (1, 1)
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
      ~resp:Conv.int ~version:1

  let v1 =
    let upgrade_req (x, y) = { x; y; others = [] } in
    let downgrade_req { x; y; others = _ } = (x, y) in
    let upgrade_resp x = No_others x in
    let downgrade_resp = function
      | No_others x -> x
      | With_others { xy; all = _ } -> xy
    in
    Decl.Request.make_gen
      ~req:(Conv.pair Conv.int Conv.int)
      ~resp:Conv.int ~upgrade_req ~downgrade_req ~upgrade_resp ~downgrade_resp
      ~version:1

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
      let from { x; y; others } = (x, y, others) in
      iso parse to_ from
    in
    let resp =
      let open Conv in
      let no_others = constr "no_others" int (fun x -> No_others x) in
      let with_others =
        constr "with_others" (pair int int) (fun (xy, all) ->
            With_others { xy; all })
      in
      sum
        [ econstr no_others; econstr with_others ]
        (function
          | No_others x -> case x no_others
          | With_others { xy; all } -> case (xy, all) with_others)
    in
    Decl.Request.make_current_gen ~req ~resp ~version:2
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
      request_exn client
        (Decl.Request.witness add_v1_v2)
        { x = 10; y = 15; others = [ -25 ] }
    in
    match resp with
    | Error _ -> assert false
    | Ok (With_others _) -> assert false
    | Ok (No_others x) -> printfn "client: %d" x
  in
  let init =
    { Initialize.Request.dune_version = (1, 9)
    ; protocol_version = Protocol.latest_version
    ; id = Id.make (Atom "test-client")
    }
  in
  test ~private_menu:[ Request add_v1_v2 ] ~init ~client ~handler ();
  [%expect
    {|
    client: sending request
    client: 25
    server: finished. |}]

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
    let+ resp =
      request_exn client (Decl.Request.witness add_v1_only) (20, 30)
    in
    match resp with
    | Error _ -> assert false
    | Ok x -> printfn "client: %d" x
  in
  let init =
    { Initialize.Request.dune_version = (1, 9)
    ; protocol_version = Protocol.latest_version
    ; id = Id.make (Atom "test-client")
    }
  in
  test ~private_menu:[ Request add_v1_only ] ~init ~client ~handler ();
  [%expect
    {|
    client: sending request
    client: 50
    server: finished. |}]

let%test_module "long polling" =
  (module struct
    let v1 =
      Decl.Request.make_current_gen ~req:Id.sexp ~resp:(Conv.option Conv.int)
        ~version:1

    let sub_proc =
      Dune_rpc.Procedures.Poll.make
        (Dune_rpc.Procedures.Poll.Name.make "pulse")
        [ v1 ]

    let sub_decl = Sub.of_procedure sub_proc

    let version = (3, 0)

    let init = init ~version ()

    let rpc () = Handler.create ~on_init ~version ()

    let server f =
      let rpc = rpc () in
      let () =
        let on_poll _session poller = f poller in
        let on_cancel _session _poll =
          printfn "server: polling cancelled";
          Fiber.return ()
        in
        Handler.Private.implement_poll rpc sub_proc ~on_poll ~on_cancel
      in
      rpc

    let server_long_poll svar =
      let rpc = rpc () in
      let () =
        Handler.implement_long_poll rpc sub_proc svar ~equal:Int.equal
          ~diff:(fun ~last ~now ->
            match last with
            | None -> now
            | Some last -> now - last)
      in
      rpc

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
        server (fun _poller ->
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
            let+ () =
              Fiber.fork_and_join_unit req (Fiber.Ivar.fill ready_to_cancel)
            in
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
  end)
