open Stdune
open Fiber.O
module Dune_rpc = Dune_rpc.Private
open Dune_rpc
open Dune_rpc_server
open Common
open Drpc

let () = Printexc.record_backtrace false
let () = Log.init No_log_file

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
  [%expect
    {|
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

let%expect_test "call method with matching versions" =
  let decl = simple_request ~method_:(Method.Name.of_string "double") Conv.int Conv.int in
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
  let decl = simple_request ~method_:(Method.Name.of_string "double") Conv.int Conv.int in
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
  let decl' =
    simple_request ~method_:(Method.Name.of_string "double") ~version:2 Conv.int Conv.int
  in
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

let add_v1_only =
  Decl.Request.make ~method_:(Method.Name.of_string "add") ~generations:[ Add.v1_only ]
;;

let add_v1_v2 =
  Decl.Request.make ~method_:(Method.Name.of_string "add") ~generations:[ Add.v1; Add.v2 ]
;;

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
  [%expect
    {|
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
  [%expect
    {|
    client: sending request
    client: 50
    server: finished. |}]
;;

let%expect_test "server to client request" =
  let decl = simple_request ~method_:(Method.Name.of_string "double") Conv.int Conv.int in
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
