open Stdune
open Fiber.O
module Dune_rpc = Dune_rpc.Private
open Dune_rpc
open Rpc.Server
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

let%expect_test "connection error includes rpc endpoint" =
  let where : Dune_rpc.Where.t = `Ip (`Host "invalid host", `Port 8587) in
  let result =
    Scheduler.run (Scheduler.create ()) (Rpc.Client.Connection.connect where)
  in
  (match result with
   | Ok _ -> assert false
   | Error message ->
     (match Stdune.User_message.to_string message |> String.split_lines with
      | [] -> ()
      | line :: _ -> print_endline line));
  [%expect {| failed to connect to RPC server tcp:host=invalid%20host,port=8587 |}]
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
   \n  [ [ \"message\"; \"Server and client have no method versions in common\" ] ]") |}]
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
      record
        (Record.make (fun x y others -> { x; y; others })
         |> Record.field "x" (required int) ~get:(fun { x; _ } -> x)
         |> Record.field "y" (required int) ~get:(fun { y; _ } -> y)
         |> Record.field "others" (required (list int)) ~get:(fun { others; _ } -> others)
         |> Record.finish)
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

let check_wire_compatibility legacy current values =
  let encode = Conv.to_sexp in
  let decode conv sexp =
    Conv.of_sexp conv ~version:(3, 0) sexp |> Result.map ~f:(encode legacy)
  in
  let all f = List.for_all values ~f in
  printfn
    "same bytes: %b; old -> new: %b; new -> old: %b"
    (all (fun value ->
       String.equal
         (Csexp.to_string (encode legacy value))
         (Csexp.to_string (encode current value))))
    (all (fun value ->
       let sexp = encode legacy value in
       Poly.equal (decode current sexp) (Ok sexp)))
    (all (fun value ->
       Poly.equal (decode legacy (encode current value)) (Ok (encode legacy value))))
;;

let%expect_test "call fields preserve the flattened wire format" =
  let legacy =
    let open Conv in
    iso
      (both (field "method" (required Method.Name.sexp)) (field "params" (required sexp)))
      (fun (method_, params) -> { Call.method_; params })
      (fun { Call.method_; params } -> method_, params)
  in
  let call =
    Call.create ~method_:(Method.Name.of_string "build") ~params:(Atom "target") ()
  in
  let with_id fields = Conv.(record (both (field "id" (required int)) fields)) in
  check_wire_compatibility (with_id legacy) (with_id Call.fields) [ 42, call ];
  Conv.to_sexp (with_id Call.fields) (42, call) |> Sexp.to_string |> print_endline;
  [%expect
    {|
    same bytes: true; old -> new: true; new -> old: true
    ((id 42) (method build) (params target)) |}]
;;

let%expect_test "diagnostic record wire compatibility" =
  let legacy =
    let open Conv in
    iso
      (record
         (eight
            (field "targets" (required (list Target.sexp)))
            (field "message" (required (Pp.sexp User_message.Style.sexp)))
            (field "loc" (optional Loc.sexp))
            (field
               "severity"
               (optional (enum [ "error", Diagnostic.Error; "warning", Warning ])))
            (field "promotion" (required (list Diagnostic.Promotion.sexp)))
            (field "directory" (optional string))
            (field "id" (required Diagnostic.Id.sexp))
            (field "related" (required (list Diagnostic.Related.sexp)))))
      (fun (targets, message, loc, severity, promotion, directory, id, related) ->
         { Diagnostic.targets; message; loc; severity; promotion; directory; id; related })
      (fun { Diagnostic.targets
           ; message
           ; loc
           ; severity
           ; promotion
           ; directory
           ; id
           ; related
           } -> targets, message, loc, severity, promotion, directory, id, related)
  in
  let start =
    { Lexing.pos_fname = "source.ml"; pos_lnum = 2; pos_bol = 10; pos_cnum = 12 }
  in
  let loc = { Loc.start; stop = { start with pos_cnum = 18 } } in
  let message = Pp.tag User_message.Style.Error (Pp.verbatim "a styled message") in
  let values =
    List.concat_map [ None; Some loc ] ~f:(fun location ->
      List.concat_map [ None; Some Diagnostic.Error; Some Warning ] ~f:(fun severity ->
        List.map [ None; Some "directory" ] ~f:(fun directory ->
          { Diagnostic.targets = [ Target.Path "target"; Alias "all" ]
          ; message
          ; loc = location
          ; severity
          ; promotion =
              [ { Diagnostic.Promotion.in_build = "build"; in_source = "source" } ]
          ; directory
          ; id = Diagnostic.Id.create 42
          ; related = [ { Diagnostic.Related.loc; message } ]
          })))
  in
  check_wire_compatibility legacy Diagnostic.sexp values;
  [%expect {| same bytes: true; old -> new: true; new -> old: true |}]
;;

let%expect_test "exported record wire formats" =
  let check conv value =
    let sexp = Conv.to_sexp conv value in
    print_endline (Sexp.to_string sexp);
    printfn
      "round trip: %b"
      (Poly.equal (Conv.of_sexp conv ~version:(3, 0) sexp) (Ok value))
  in
  let start = { Lexing.pos_fname = "a.ml"; pos_lnum = 2; pos_bol = 10; pos_cnum = 12 } in
  let loc =
    { Loc.start
    ; stop = { Lexing.pos_fname = "b.ml"; pos_lnum = 3; pos_bol = 20; pos_cnum = 25 }
    }
  in
  check Loc.sexp loc;
  [%expect
    {|
    ((start ((pos_bol 10) (pos_cnum 12) (pos_fname a.ml) (pos_lnum 2))) (stop ((pos_bol 20) (pos_cnum 25) (pos_fname b.ml) (pos_lnum 3))))
    round trip: true |}];
  check
    Diagnostic.Promotion.sexp
    { Diagnostic.Promotion.in_build = "_build/a"; in_source = "a" };
  [%expect
    {|
    ((in_build _build/a) (in_source a))
    round trip: true |}];
  check
    Diagnostic.Related.sexp
    { Diagnostic.Related.loc
    ; message = Pp.tag User_message.Style.Hint (Pp.verbatim "hint")
    };
  [%expect
    {|
    ((loc ((start ((pos_bol 10) (pos_cnum 12) (pos_fname a.ml) (pos_lnum 2))) (stop ((pos_bol 20) (pos_cnum 25) (pos_fname b.ml) (pos_lnum 3))))) (message (Tag ((Hint ()) (Verbatim hint)))))
    round trip: true |}];
  List.iter [ None; Some (Sexp.Atom "payload") ] ~f:(fun payload ->
    check Message.sexp { Message.payload; message = "message" });
  [%expect
    {|
    ((message message))
    round trip: true
    ((message message) (payload payload))
    round trip: true |}];
  let id = Job.Id.create 42 in
  check
    Job.Event.sexp
    (Job.Event.Start
       { Job.id
       ; pid = 123
       ; description = Pp.tag () (Pp.verbatim "job")
       ; started_at = 1.5
       });
  check Job.Event.sexp (Job.Event.Stop id);
  [%expect
    {|
    (Start ((description (Tag (Verbatim job))) (id 42) (pid 123) (started_at 1.5)))
    round trip: true
    (Stop 42)
    round trip: true |}];
  List.iter
    [ Files_to_promote.All; These [ Stdune.Path.Source.of_string "a" ] ]
    ~f:(fun files ->
      List.iter [ Promote_targets.Matching.Exact; Prefix ] ~f:(fun matching ->
        check Promote_targets.sexp { Promote_targets.files; matching }));
  [%expect
    {|
    ((files ()) (matching exact))
    round trip: true
    ((files ()) (matching prefix))
    round trip: true
    ((files (a)) (matching exact))
    round trip: true
    ((files (a)) (matching prefix))
    round trip: true |}]
;;

let%expect_test "RPC protocol record wire formats" =
  let check conv value =
    let sexp = Conv.to_sexp conv value in
    print_endline (Sexp.to_string sexp);
    printfn
      "round trip: %b"
      (Poly.equal (Conv.of_sexp conv ~version:(3, 0) sexp) (Ok value))
  in
  let response = Conv.record Response.fields in
  let id = Id.make (Atom "request") in
  List.iter [ Response.Error.Invalid_request; Code_error ] ~f:(fun kind ->
    List.iter [ None; Some (Sexp.Atom "payload") ] ~f:(fun payload ->
      check response (id, Error { Response.Error.kind; payload; message = "message" })));
  [%expect
    {|
    ((id request) (result (error ((kind Invalid_request) (message message)))))
    round trip: true
    ((id request) (result (error ((kind Invalid_request) (message message) (payload payload)))))
    round trip: true
    ((id request) (result (error ((kind Code_error) (message message)))))
    round trip: true
    ((id request) (result (error ((kind Code_error) (message message) (payload payload)))))
    round trip: true |}];
  let request = { Initialize.Request.dune_version = 3, 0; protocol_version = 0; id } in
  let call = Initialize.Request.to_call request in
  Conv.to_sexp (Conv.record Call.fields) call |> Sexp.to_string |> print_endline;
  printfn
    "round trip: %b"
    (Poly.equal (Initialize.Request.of_call call ~version:(3, 0)) (Ok request));
  [%expect
    {|
    ((method initialize) (params ((dune_version (3 0)) (id request) (protocol_version 0))))
    round trip: true |}];
  let module Initialize_response = Procedures.Public.Action_plugin.Initialize_response in
  check Initialize_response.conv { Initialize_response.root = "/sandbox/default" };
  [%expect
    {|
    ((root /sandbox/default))
    round trip: true |}]
;;

let%expect_test "format and add request wire formats" =
  let check (_, Decl.Generation.T { req; upgrade_req; downgrade_req; _ }) value =
    let sexp = Conv.to_sexp req (downgrade_req value) in
    print_endline (Sexp.to_string sexp);
    let decoded = Conv.of_sexp req ~version:(3, 0) sexp |> Result.map ~f:upgrade_req in
    printfn "round trip: %b" (Poly.equal decoded (Ok value))
  in
  List.iter Procedures.Public.format_dune_file.generations ~f:(fun gen ->
    check gen ("dune", `Contents "contents"));
  [%expect
    {|
    ((contents contents) (path dune))
    round trip: true |}];
  check Add.v2 { Add.x = 2; y = 7; others = [ -1; 3 ] };
  [%expect
    {|
    ((others (-1 3)) (x 2) (y 7))
    round trip: true |}]
;;

let%expect_test "V1 diagnostic record wire format" =
  let start = { Lexing.pos_fname = "a.ml"; pos_lnum = 2; pos_bol = 10; pos_cnum = 12 } in
  let loc = { Loc.start; stop = { start with pos_cnum = 18 } } in
  let message = Pp.tag User_message.Style.Details (Pp.verbatim "message") in
  let minimal =
    { Diagnostic.id = Diagnostic.Id.create 7
    ; message
    ; targets = []
    ; loc = None
    ; severity = None
    ; promotion = []
    ; directory = None
    ; related = []
    }
  in
  let full =
    { minimal with
      targets = [ Target.Alias "all" ]
    ; loc = Some loc
    ; severity = Some Warning
    ; promotion = [ { Diagnostic.Promotion.in_build = "build"; in_source = "source" } ]
    ; directory = Some "dir"
    ; related = [ { Diagnostic.Related.loc; message } ]
    }
  in
  List.iter
    Procedures.Public.diagnostics.generations
    ~f:(fun (version, Decl.Generation.T { resp; upgrade_resp; downgrade_resp; _ }) ->
      if version = 1
      then
        List.iter [ minimal; full ] ~f:(fun diagnostic ->
          let value = [ diagnostic ] in
          let sexp = Conv.to_sexp resp (downgrade_resp value) in
          print_endline (Sexp.to_string sexp);
          let decoded =
            Conv.of_sexp resp ~version:(3, 0) sexp |> Result.map ~f:upgrade_resp
          in
          printfn "round trip: %b" (Poly.equal decoded (Ok value))));
  [%expect
    {|
    (((id 7) (message (Tag (Verbatim message))) (promotion ()) (related ()) (targets ())))
    round trip: true
    (((directory dir) (id 7) (loc ((start ((pos_bol 10) (pos_cnum 12) (pos_fname a.ml) (pos_lnum 2))) (stop ((pos_bol 10) (pos_cnum 18) (pos_fname a.ml) (pos_lnum 2))))) (message (Tag (Verbatim message))) (promotion (((in_build build) (in_source source)))) (related (((loc ((start ((pos_bol 10) (pos_cnum 12) (pos_fname a.ml) (pos_lnum 2))) (stop ((pos_bol 10) (pos_cnum 18) (pos_fname a.ml) (pos_lnum 2))))) (message (Tag (Verbatim message)))))) (severity warning) (targets ((Alias all)))))
    round trip: true |}]
;;
