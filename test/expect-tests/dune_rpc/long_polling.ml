open Stdune
open Fiber.O
module Dune_rpc = Dune_rpc.Private
open Dune_rpc
open Common
open Drpc

let () = Printexc.record_backtrace false

let init ?(id = Id.make (Csexp.Atom "test-client")) ?(version = 1, 1) () =
  { Initialize.Request.dune_version = version
  ; protocol_version = Protocol.latest_version
  ; id
  }
;;

let v1 =
  Decl.Request.make_current_gen ~req:Id.sexp ~resp:(Conv.option Conv.int) ~version:1
;;

let sub_proc =
  Dune_rpc.Procedures.Poll.make (Dune_rpc.Procedures.Poll.Name.make "pulse") [ v1 ]
;;

let sub_decl = Sub.of_procedure sub_proc
let version = 3, 0
let init = init ~version ()
let rpc () = Rpc.Server.Handler.create ~on_init ~version ()

let server f =
  let rpc = rpc () in
  let () =
    let on_poll _session = f () in
    let on_cancel _session =
      printfn "server: polling cancelled";
      Fiber.return ()
    in
    Rpc.Server.Handler.For_tests.implement_poll rpc sub_proc ~on_poll ~on_cancel
  in
  rpc
;;

let server_long_poll svar =
  let rpc = rpc () in
  let () =
    Rpc.Server.Handler.implement_long_poll
      rpc
      sub_proc
      (Rpc.Long_poll.Source.Svar svar)
      ~equal:Int.equal
      ~diff:(fun ~last ~now ->
        match last with
        | None -> now
        | Some last -> now - last)
  in
  rpc
;;

let poll_exn ?id client =
  Client.poll ?id client sub_decl
  >>| function
  | Ok poller -> poller
  | Error e -> raise (Version_error.E e)
;;

let print_next label poller =
  Client.Stream.next poller
  >>| function
  | None -> printfn "client: %s no more values" label
  | Some a -> printfn "client: %s received %d" label a
;;

let ping_decl = simple_request ~method_:(Method.Name.of_string "ping") Conv.unit Conv.int
let ping_witness = Decl.Request.witness ping_decl

let add_ping rpc =
  Rpc.Server.Handler.implement_request rpc ping_decl (fun _ () -> Fiber.return 42)
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

(* A long poll backed by a [Computed] source reads its state from a plain [ref] and polls
   it for changes. Here we mutate the ref before asking for the next value, so the poll's
   predicate is already satisfied and the source returns immediately without sleeping. *)
let server_long_poll_computed r =
  let rpc = rpc () in
  let () =
    Rpc.Server.Handler.implement_long_poll
      rpc
      sub_proc
      (Rpc.Long_poll.Source.Computed
         { get = (fun () -> !r); poll_every = Time.Span.of_secs 0.01 })
      ~equal:Int.equal
      ~diff:(fun ~last ~now ->
        match last with
        | None -> now
        | Some last -> now - last)
  in
  rpc
;;

let%expect_test "long polling - computed (poll/ref) source" =
  let r = ref 0 in
  let handler = server_long_poll_computed r in
  let client client =
    let* poller = poll_exn client in
    r := 1;
    let* () = print_next "first" poller in
    r := 5;
    let* () = print_next "second" poller in
    Client.Stream.cancel poller
  in
  test ~init ~client ~handler ~private_menu:[ Poll sub_proc ] ();
  [%expect
    {|
    client: first received 1
    client: second received 4
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
  [%expect
    {|
    client: received 1
    client: waiting for second value (that will never come)
    client: cancelling
    client: no more values
    client: finishing session
    server: finished.
    |}]
;;

let%expect_test "long polling - connection remains usable after in-flight cancel" =
  let ready_to_cancel : unit Fiber.Ivar.t = Fiber.Ivar.create () in
  let svar = Fiber.Svar.create 0 in
  let handler =
    let rpc = server_long_poll svar in
    add_ping rpc;
    rpc
  in
  let client client =
    let* poller = poll_exn client in
    let* () = Fiber.Svar.write svar 1 in
    let* () = print_next "poll" poller in
    let* () =
      Fiber.fork_and_join_unit
        (fun () ->
           let* () = Fiber.Ivar.read ready_to_cancel in
           printfn "client: cancelling";
           Client.Stream.cancel poller)
        (fun () ->
           printfn "client: waiting for second value";
           let+ () =
             Fiber.fork_and_join_unit
               (fun () -> print_next "poll" poller)
               (Fiber.Ivar.fill ready_to_cancel)
           in
           printfn "client: cancelled poll returned")
    in
    request_exn client ping_witness ()
    >>| function
    | Ok n -> printfn "client: ping %d" n
    | Error error -> printfn "%s" (Dyn.to_string (Response.Error.to_dyn error))
  in
  test ~init ~client ~handler ~private_menu:[ Poll sub_proc; Request ping_decl ] ();
  [%expect
    {|
    client: poll received 1
    client: waiting for second value
    client: cancelling
    client: poll no more values
    client: cancelled poll returned
    client: ping 42
    server: finished.
    |}]
;;

let%expect_test "long polling - cancelled explicit poll id can be reused" =
  let ready_to_cancel : unit Fiber.Ivar.t = Fiber.Ivar.create () in
  let svar = Fiber.Svar.create 0 in
  let handler = server_long_poll svar in
  let poll_id = Id.make (Atom "reused-poll") in
  let client client =
    let* poller = poll_exn ~id:poll_id client in
    let* () = Fiber.Svar.write svar 1 in
    let* () = print_next "first poll" poller in
    let* () =
      Fiber.fork_and_join_unit
        (fun () ->
           let* () = Fiber.Ivar.read ready_to_cancel in
           printfn "client: cancelling first poll";
           Client.Stream.cancel poller)
        (fun () ->
           printfn "client: waiting on first poll";
           let+ () =
             Fiber.fork_and_join_unit
               (fun () -> print_next "first poll" poller)
               (Fiber.Ivar.fill ready_to_cancel)
           in
           printfn "client: first poll cancelled")
    in
    let* poller = poll_exn ~id:poll_id client in
    let* () = Fiber.Svar.write svar 2 in
    print_next "second poll" poller
  in
  test ~init ~client ~handler ~private_menu:[ Poll sub_proc ] ();
  [%expect
    {|
    client: first poll received 1
    client: waiting on first poll
    client: cancelling first poll
    client: first poll no more values
    client: first poll cancelled
    client: second poll received 2
    server: finished.
    |}]
;;

let%expect_test "long polling - late response after cancel is ignored" =
  let poll_started : unit Fiber.Ivar.t = Fiber.Ivar.create () in
  let release_response : unit Fiber.Ivar.t = Fiber.Ivar.create () in
  let handler =
    let rpc = rpc () in
    let on_poll _session =
      let* () = Fiber.Ivar.fill poll_started () in
      let+ () = Fiber.Ivar.read release_response in
      printfn "server: sending late response";
      Some 7
    in
    let on_cancel _session =
      printfn "server: polling cancelled";
      Fiber.Ivar.fill release_response ()
    in
    Rpc.Server.Handler.For_tests.implement_poll rpc sub_proc ~on_poll ~on_cancel;
    add_ping rpc;
    rpc
  in
  let client client =
    let* poller = poll_exn client in
    let* () =
      Fiber.fork_and_join_unit
        (fun () -> print_next "poll" poller)
        (fun () ->
           let* () = Fiber.Ivar.read poll_started in
           printfn "client: cancelling";
           Client.Stream.cancel poller)
    in
    request_exn client ping_witness ()
    >>| function
    | Ok n -> printfn "client: ping %d" n
    | Error error -> printfn "%s" (Dyn.to_string (Response.Error.to_dyn error))
  in
  test ~init ~client ~handler ~private_menu:[ Poll sub_proc; Request ping_decl ] ();
  [%expect
    {|
    client: cancelling
    client: poll no more values
    server: polling cancelled
    server: sending late response
    client: ping 42
    server: finished.
    |}]
;;

let%expect_test "long polling - session close cancels in-flight poll" =
  let svar = Fiber.Svar.create 0 in
  let handler = server_long_poll svar in
  let run =
    let client_chan, sessions = setup_direct_client_server () in
    let client () =
      Client.connect_with_menu
        client_chan
        init
        ~private_menu:[ Poll sub_proc ]
        ~f:(fun client ->
          let* poller = poll_exn client in
          let* () = Fiber.Svar.write svar 1 in
          let* () = print_next "poll" poller in
          Fiber.fork_and_join_unit
            (fun () -> print_next "poll" poller)
            (fun () ->
               printfn "client: closing channel";
               Chan.close client_chan))
    in
    let server () =
      let+ () = Server.serve sessions (Rpc.Server.make handler) in
      printfn "server: finished."
    in
    Fiber.parallel_iter [ client; server ] ~f:(fun f -> f ())
  in
  Scheduler.run (Scheduler.create ()) run;
  [%expect
    {|
    client: poll received 1
    client: closing channel
    server: finished.
    client: poll no more values
    |}]
;;

let%expect_test "long polling - session close retains idle poller state" =
  let current = ref None in
  let weak = Weak.create 1 in
  let set_initial_state () =
    let state = ref 0 in
    Weak.set weak 0 (Some state);
    current := Some state
  in
  let set_next_state () = current := Some (ref 1) in
  let read_current () =
    match !current with
    | Some state -> state
    | None -> Code_error.raise "missing state" []
  in
  let handler =
    let rpc = rpc () in
    Rpc.Server.Handler.implement_long_poll
      rpc
      sub_proc
      (Rpc.Long_poll.Source.Computed
         { get = read_current; poll_every = Time.Span.of_secs 0.01 })
      ~equal:( == )
      ~diff:(fun ~last:_ ~now:_ -> 0);
    rpc
  in
  let run =
    let client_chan, sessions = setup_direct_client_server () in
    let client () =
      Client.connect_with_menu
        client_chan
        init
        ~private_menu:[ Poll sub_proc ]
        ~f:(fun client ->
          set_initial_state ();
          let* poller = poll_exn client in
          let* (_ : int option) = Client.Stream.next poller in
          set_next_state ();
          Chan.close client_chan)
    in
    let server () = Server.serve sessions (Rpc.Server.make handler) in
    Fiber.parallel_iter [ client; server ] ~f:(fun f -> f ())
  in
  Scheduler.run (Scheduler.create ()) run;
  let rec collect = function
    | 0 -> printfn "state retained"
    | n ->
      Gc.full_major ();
      (match Weak.get weak 0 with
       | None -> printfn "state collected"
       | Some _ -> collect (n - 1))
  in
  collect 10;
  ignore (Sys.opaque_identity handler);
  [%expect {| state retained |}]
;;

let%expect_test "long polling - cancelling one poller does not stop another" =
  let svar = Fiber.Svar.create 0 in
  let handler = server_long_poll svar in
  let client client =
    let* first = poll_exn client in
    let* second = poll_exn client in
    let* () = Fiber.Svar.write svar 1 in
    let* () = print_next "first poll" first in
    let* () = print_next "second poll" second in
    let* () = print_next "first poll" first
    and+ () = print_next "second poll" second
    and+ () =
      printfn "client: cancelling first poll";
      let* () = Client.Stream.cancel first in
      printfn "client: updating state";
      Fiber.Svar.write svar 2
    in
    Fiber.return ()
  in
  test ~init ~client ~handler ~private_menu:[ Poll sub_proc ] ();
  [%expect
    {|
    client: first poll received 1
    client: second poll received 1
    client: cancelling first poll
    client: updating state
    client: first poll no more values
    client: second poll received 1
    server: finished.
    |}]
;;
