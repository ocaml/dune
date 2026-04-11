open Stdune
open Fiber.O
module Dune_rpc = Dune_rpc.Private
open Dune_rpc
open Dune_rpc_server
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
