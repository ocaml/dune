open Stdune
open Fiber.O
open Dune_rpc_e2e
module Dune_rpc = Dune_rpc_private
module Sub = Dune_rpc.Sub
module Client = Dune_rpc_impl.Client
module Diagnostic = Dune_rpc.Diagnostic
module Request = Dune_rpc.Public.Request
module Response = Dune_rpc.Response

let%expect_test "turn on and shutdown" =
  let test () =
    with_dune_watch (fun _pid ->
        run_client (fun client ->
            let+ () = dune_build client "." in
            printfn "shutting down"))
  in
  run test;
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Dune_util__Report_error.Already_reported)
  Raised at Dune_engine__Scheduler.Run.go.(fun) in file "src/dune_engine/scheduler.ml", line 1355, characters 27-40
  Called from Stdune__Exn.protectx in file "otherlibs/stdune/exn.ml", line 12, characters 8-11
  Re-raised at Stdune__Exn.protectx in file "otherlibs/stdune/exn.ml", line 18, characters 4-11
  Called from Dune_rpc_diagnostics.(fun) in file "test/expect-tests/dune_rpc_e2e/dune_rpc_diagnostics.ml", line 18, characters 2-10
  Called from Expect_test_collector.Make.Instance.exec in file "collector/expect_test_collector.ml", line 244, characters 12-19

  Trailing output
  ---------------

  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  Building .
  Error building .:
  { payload = Some [ [ "id"; [ "auto"; "0" ] ] ]
  ; message =
      "connection terminated. this request will never receive a response"
  ; kind = Code_error
  }
  shutting down
  /-----------------------------------------------------------------------
  | Internal error: Uncaught exception.
  | Response.E
  |   { payload = Some [ [ "method"; "shutdown" ]; [ "params"; [] ] ]
  |   ; message = "notification sent while connection is dead"
  |   ; kind = Code_error
  |   }
  | Raised at Dune_rpc_private.Client.Make.make_notification in file "otherlibs/dune-rpc/private/dune_rpc_private.ml", line 429, characters 8-36
  | Called from Fiber.Scheduler.exec in file "src/fiber/fiber.ml", line 878, characters 10-13
  | Re-raised at Stdune__Exn.raise_with_backtrace in file "otherlibs/stdune/exn.ml", line 36, characters 27-56
  | Called from Fiber.Scheduler.exec in file "src/fiber/fiber.ml", line 878, characters 10-13
  | Re-raised at Stdune__Exn.raise_with_backtrace in file "otherlibs/stdune/exn.ml", line 36, characters 27-56
  | Called from Fiber.Scheduler.exec in file "src/fiber/fiber.ml", line 878, characters 10-13
  \-----------------------------------------------------------------------
  $PATH/dune build --passive-watch-mode --root . received signal -7
  Error: exception Response.E
    { payload = Some [ [ "method"; "shutdown" ]; [ "params"; [] ] ]
    ; message = "notification sent while connection is dead"
    ; kind = Code_error
    }
  Raised at Dune_rpc_private.Client.Make.make_notification in file
    "otherlibs/dune-rpc/private/dune_rpc_private.ml", line 429, characters 8-36
  Called from Fiber.Scheduler.exec in file "src/fiber/fiber.ml", line 878,
    characters 10-13
  Re-raised at Stdune__Exn.raise_with_backtrace in file
    "otherlibs/stdune/exn.ml", line 36, characters 27-56
  Called from Fiber.Scheduler.exec in file "src/fiber/fiber.ml", line 878,
    characters 10-13
  Re-raised at Stdune__Exn.raise_with_backtrace in file
    "otherlibs/stdune/exn.ml", line 36, characters 27-56
  Called from Fiber.Scheduler.exec in file "src/fiber/fiber.ml", line 878,
    characters 10-13
  Re-raised at Stdune__Exn.raise_with_backtrace in file
    "otherlibs/stdune/exn.ml", line 36, characters 27-56
  Called from Fiber.Scheduler.exec in file "src/fiber/fiber.ml", line 878,
    characters 10-13
  Re-raised at Stdune__Exn.raise_with_backtrace in file
    "otherlibs/stdune/exn.ml", line 36, characters 27-56
  Called from Fiber.Scheduler.exec in file "src/fiber/fiber.ml", line 878,
    characters 10-13

  I must not crash.  Uncertainty is the mind-killer. Exceptions are the
  little-death that brings total obliteration.  I will fully express my cases.
  Execution will pass over me and through me.  And when it has gone past, I
  will unwind the stack along its path.  Where the cases are handled there will
  be nothing.  Only I will remain. |}]

let files =
  List.iter ~f:(fun (f, contents) -> Io.String_path.write_file f contents)

let on_diagnostic_event diagnostics =
  let cwd = Sys.getcwd () in
  let sanitize_path path =
    match String.drop_prefix path ~prefix:cwd with
    | None -> path
    | Some s -> "$CWD" ^ s
  in
  let sanitize_pp pp = Pp.verbatim (Format.asprintf "%a@." Pp.to_fmt pp) in
  let sanitize_loc =
    let sanitize_position (p : Lexing.position) =
      { p with pos_fname = sanitize_path p.pos_fname }
    in
    fun (loc : Loc.t) ->
      { Loc.start = sanitize_position loc.start
      ; stop = sanitize_position loc.stop
      }
  in
  (* function to remove remove pp tags and hide junk from paths *)
  let map_event (d : Diagnostic.Event.t) f : Diagnostic.Event.t =
    match d with
    | Remove e -> Remove (f e)
    | Add e -> Add (f e)
  in
  let sanitize (d : Diagnostic.t) =
    let directory = Option.map d.directory ~f:sanitize_path in
    let promotion =
      List.map d.promotion ~f:(fun (p : Diagnostic.Promotion.t) ->
          let in_build = sanitize_path p.in_build in
          let in_source = sanitize_path p.in_source in
          { Diagnostic.Promotion.in_build; in_source })
    in
    let related =
      List.map d.related ~f:(fun (related : Diagnostic.Related.t) ->
          let loc = sanitize_loc related.loc in
          let message = sanitize_pp related.message in
          { Diagnostic.Related.message; loc })
    in
    { d with
      message = sanitize_pp d.message
    ; loc = Option.map d.loc ~f:sanitize_loc
    ; directory
    ; promotion
    ; related
    }
  in
  if List.is_empty diagnostics then
    print_endline "<no diagnostics>"
  else
    List.iter diagnostics ~f:(fun (e : Diagnostic.Event.t) ->
        (match e with
        | Remove _ -> ()
        | Add e ->
          Diagnostic.promotion e
          |> List.iter ~f:(fun promotion ->
                 let path = Diagnostic.Promotion.in_build promotion in
                 if not (Sys.file_exists path) then
                   printfn "FAILURE: promotion file %s does not exist"
                     (sanitize_path path)));
        let e = map_event e sanitize in
        printfn "%s" (Dyn.to_string (Diagnostic.Event.to_dyn e)))

let setup_diagnostics f =
  let exec _pid =
    run_client (fun client ->
        (* First we test for regular errors *)
        files [ ("dune-project", "(lang dune 3.0)") ];
        f client)
  in
  run (fun () -> with_dune_watch exec)

let poll_exn client decl =
  let+ poll = Client.poll client decl in
  match poll with
  | Ok p -> p
  | Error e -> raise (Dune_rpc.Version_error.E e)

let print_diagnostics poll =
  let+ res = Client.Stream.next poll in
  match res with
  | None -> printfn "client: no more diagnostics"
  | Some diag -> on_diagnostic_event diag

let diagnostic_with_build setup target =
  let exec _pid =
    run_client (fun client ->
        (* First we test for regular errors *)
        files (("dune-project", "(lang dune 3.0)") :: setup);
        let* () = dune_build client target in
        let* poll = poll_exn client Dune_rpc.Public.Sub.diagnostic in
        let* () = print_diagnostics poll in
        Client.Stream.cancel poll)
  in
  run (fun () -> with_dune_watch exec)

let%expect_test "error in dune file" =
  diagnostic_with_build [ ("dune", "(library (name foo))") ] "foo.cma";
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Dune_util__Report_error.Already_reported)
  Raised at Dune_engine__Scheduler.Run.go.(fun) in file "src/dune_engine/scheduler.ml", line 1355, characters 27-40
  Called from Stdune__Exn.protectx in file "otherlibs/stdune/exn.ml", line 12, characters 8-11
  Re-raised at Stdune__Exn.protectx in file "otherlibs/stdune/exn.ml", line 18, characters 4-11
  Called from Dune_rpc_diagnostics.(fun) in file "test/expect-tests/dune_rpc_e2e/dune_rpc_diagnostics.ml", line 125, characters 2-70
  Called from Expect_test_collector.Make.Instance.exec in file "collector/expect_test_collector.ml", line 244, characters 12-19

  Trailing output
  ---------------

  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  Building foo.cma
  Error building foo.cma:
  { payload = Some [ [ "id"; [ "auto"; "0" ] ] ]
  ; message =
      "connection terminated. this request will never receive a response"
  ; kind = Code_error
  }
  /-----------------------------------------------------------------------
  | Internal error: Uncaught exception.
  | Response.E
  |   { payload =
  |       Some
  |         [ [ "id"; [ [ "poll"; [ "auto"; "1" ] ]; [ "i"; "0" ] ] ]
  |         ; [ "req"
  |           ; [ [ "method"; "poll/diagnostic" ]
  |             ; [ "params"; [ "auto"; "1" ] ]
  |             ]
  |           ]
  |         ]
  |   ; message = "request sent while connection is dead"
  |   ; kind = Code_error
  |   }
  | Raised at Dune_rpc_private.Client.Make.Stream.next in file "otherlibs/dune-rpc/private/dune_rpc_private.ml", line 487, characters 10-36
  | Called from Fiber.O.(>>|).(fun) in file "src/fiber/fiber.ml", line 255, characters 36-41
  | Called from Fiber.Scheduler.exec in file "src/fiber/fiber.ml", line 878, characters 10-13
  \-----------------------------------------------------------------------
  /-----------------------------------------------------------------------
  | Internal error: Uncaught exception.
  | Response.E
  |   { payload = Some [ [ "method"; "shutdown" ]; [ "params"; [] ] ]
  |   ; message = "notification sent while connection is dead"
  |   ; kind = Code_error
  |   }
  | Raised at Dune_rpc_private.Client.Make.make_notification in file "otherlibs/dune-rpc/private/dune_rpc_private.ml", line 429, characters 8-36
  | Called from Fiber.Scheduler.exec in file "src/fiber/fiber.ml", line 878, characters 10-13
  \-----------------------------------------------------------------------
  $PATH/dune build --passive-watch-mode --root . received signal -7
  Error: exception Response.E
    { payload =
        Some
          [ [ "id"; [ [ "poll"; [ "auto"; "1" ] ]; [ "i"; "0" ] ] ]
          ; [ "req"
            ; [ [ "method"; "poll/diagnostic" ]
              ; [ "params"; [ "auto"; "1" ] ]
              ]
            ]
          ]
    ; message = "request sent while connection is dead"
    ; kind = Code_error
    }
  Raised at Dune_rpc_private.Client.Make.Stream.next in file
    "otherlibs/dune-rpc/private/dune_rpc_private.ml", line 487, characters
    10-36
  Called from Fiber.O.(>>|).(fun) in file "src/fiber/fiber.ml", line 255,
    characters 36-41
  Called from Fiber.Scheduler.exec in file "src/fiber/fiber.ml", line 878,
    characters 10-13
  Re-raised at Stdune__Exn.raise_with_backtrace in file
    "otherlibs/stdune/exn.ml", line 36, characters 27-56
  Called from Fiber.Scheduler.exec in file "src/fiber/fiber.ml", line 878,
    characters 10-13
  Error: exception Response.E
    { payload = Some [ [ "method"; "shutdown" ]; [ "params"; [] ] ]
    ; message = "notification sent while connection is dead"
    ; kind = Code_error
    }
  Raised at Dune_rpc_private.Client.Make.make_notification in file
    "otherlibs/dune-rpc/private/dune_rpc_private.ml", line 429, characters 8-36
  Called from Fiber.Scheduler.exec in file "src/fiber/fiber.ml", line 878,
    characters 10-13
  Re-raised at Stdune__Exn.raise_with_backtrace in file
    "otherlibs/stdune/exn.ml", line 36, characters 27-56
  Called from Fiber.Scheduler.exec in file "src/fiber/fiber.ml", line 878,
    characters 10-13 |}]

let%expect_test "related error" =
  diagnostic_with_build
    [ ("dune", "(library (name foo))")
    ; ("foo.mli", "val x : int")
    ; ("foo.ml", "let x = true")
    ]
    "foo.cma";
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Dune_util__Report_error.Already_reported)
  Raised at Dune_engine__Scheduler.Run.go.(fun) in file "src/dune_engine/scheduler.ml", line 1355, characters 27-40
  Called from Stdune__Exn.protectx in file "otherlibs/stdune/exn.ml", line 12, characters 8-11
  Re-raised at Stdune__Exn.protectx in file "otherlibs/stdune/exn.ml", line 18, characters 4-11
  Called from Dune_rpc_diagnostics.(fun) in file "test/expect-tests/dune_rpc_e2e/dune_rpc_diagnostics.ml", line 135, characters 2-148
  Called from Expect_test_collector.Make.Instance.exec in file "collector/expect_test_collector.ml", line 244, characters 12-19

  Trailing output
  ---------------

  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  Building foo.cma
  Error building foo.cma:
  { payload = Some [ [ "id"; [ "auto"; "0" ] ] ]
  ; message =
      "connection terminated. this request will never receive a response"
  ; kind = Code_error
  }
  /-----------------------------------------------------------------------
  | Internal error: Uncaught exception.
  | Response.E
  |   { payload =
  |       Some
  |         [ [ "id"; [ [ "poll"; [ "auto"; "1" ] ]; [ "i"; "0" ] ] ]
  |         ; [ "req"
  |           ; [ [ "method"; "poll/diagnostic" ]
  |             ; [ "params"; [ "auto"; "1" ] ]
  |             ]
  |           ]
  |         ]
  |   ; message = "request sent while connection is dead"
  |   ; kind = Code_error
  |   }
  | Raised at Dune_rpc_private.Client.Make.Stream.next in file "otherlibs/dune-rpc/private/dune_rpc_private.ml", line 487, characters 10-36
  | Called from Fiber.O.(>>|).(fun) in file "src/fiber/fiber.ml", line 255, characters 36-41
  | Called from Fiber.Scheduler.exec in file "src/fiber/fiber.ml", line 878, characters 10-13
  \-----------------------------------------------------------------------
  /-----------------------------------------------------------------------
  | Internal error: Uncaught exception.
  | Response.E
  |   { payload = Some [ [ "method"; "shutdown" ]; [ "params"; [] ] ]
  |   ; message = "notification sent while connection is dead"
  |   ; kind = Code_error
  |   }
  | Raised at Dune_rpc_private.Client.Make.make_notification in file "otherlibs/dune-rpc/private/dune_rpc_private.ml", line 429, characters 8-36
  | Called from Fiber.Scheduler.exec in file "src/fiber/fiber.ml", line 878, characters 10-13
  \-----------------------------------------------------------------------
  $PATH/dune build --passive-watch-mode --root . received signal -7
  Error: exception Response.E
    { payload =
        Some
          [ [ "id"; [ [ "poll"; [ "auto"; "1" ] ]; [ "i"; "0" ] ] ]
          ; [ "req"
            ; [ [ "method"; "poll/diagnostic" ]
              ; [ "params"; [ "auto"; "1" ] ]
              ]
            ]
          ]
    ; message = "request sent while connection is dead"
    ; kind = Code_error
    }
  Raised at Dune_rpc_private.Client.Make.Stream.next in file
    "otherlibs/dune-rpc/private/dune_rpc_private.ml", line 487, characters
    10-36
  Called from Fiber.O.(>>|).(fun) in file "src/fiber/fiber.ml", line 255,
    characters 36-41
  Called from Fiber.Scheduler.exec in file "src/fiber/fiber.ml", line 878,
    characters 10-13
  Re-raised at Stdune__Exn.raise_with_backtrace in file
    "otherlibs/stdune/exn.ml", line 36, characters 27-56
  Called from Fiber.Scheduler.exec in file "src/fiber/fiber.ml", line 878,
    characters 10-13
  Error: exception Response.E
    { payload = Some [ [ "method"; "shutdown" ]; [ "params"; [] ] ]
    ; message = "notification sent while connection is dead"
    ; kind = Code_error
    }
  Raised at Dune_rpc_private.Client.Make.make_notification in file
    "otherlibs/dune-rpc/private/dune_rpc_private.ml", line 429, characters 8-36
  Called from Fiber.Scheduler.exec in file "src/fiber/fiber.ml", line 878,
    characters 10-13
  Re-raised at Stdune__Exn.raise_with_backtrace in file
    "otherlibs/stdune/exn.ml", line 36, characters 27-56
  Called from Fiber.Scheduler.exec in file "src/fiber/fiber.ml", line 878,
    characters 10-13 |}]

let%expect_test "promotion" =
  diagnostic_with_build
    [ ( "dune"
      , {|
(rule (alias foo) (action (diff x x.gen)))
(rule (with-stdout-to x.gen (echo "toto")))
|}
      )
    ; ("x", "titi")
    ]
    "(alias foo)";
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Dune_util__Report_error.Already_reported)
  Raised at Dune_engine__Scheduler.Run.go.(fun) in file "src/dune_engine/scheduler.ml", line 1355, characters 27-40
  Called from Stdune__Exn.protectx in file "otherlibs/stdune/exn.ml", line 12, characters 8-11
  Re-raised at Stdune__Exn.protectx in file "otherlibs/stdune/exn.ml", line 18, characters 4-11
  Called from Dune_rpc_diagnostics.(fun) in file "test/expect-tests/dune_rpc_e2e/dune_rpc_diagnostics.ml", line 231, characters 2-191
  Called from Expect_test_collector.Make.Instance.exec in file "collector/expect_test_collector.ml", line 244, characters 12-19

  Trailing output
  ---------------

  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  Building (alias foo)
  Error building (alias foo):
  { payload = Some [ [ "id"; [ "auto"; "0" ] ] ]
  ; message =
      "connection terminated. this request will never receive a response"
  ; kind = Code_error
  }
  /-----------------------------------------------------------------------
  | Internal error: Uncaught exception.
  | Response.E
  |   { payload =
  |       Some
  |         [ [ "id"; [ [ "poll"; [ "auto"; "1" ] ]; [ "i"; "0" ] ] ]
  |         ; [ "req"
  |           ; [ [ "method"; "poll/diagnostic" ]
  |             ; [ "params"; [ "auto"; "1" ] ]
  |             ]
  |           ]
  |         ]
  |   ; message = "request sent while connection is dead"
  |   ; kind = Code_error
  |   }
  | Raised at Dune_rpc_private.Client.Make.Stream.next in file "otherlibs/dune-rpc/private/dune_rpc_private.ml", line 487, characters 10-36
  | Called from Fiber.O.(>>|).(fun) in file "src/fiber/fiber.ml", line 255, characters 36-41
  | Called from Fiber.Scheduler.exec in file "src/fiber/fiber.ml", line 878, characters 10-13
  \-----------------------------------------------------------------------
  /-----------------------------------------------------------------------
  | Internal error: Uncaught exception.
  | Response.E
  |   { payload = Some [ [ "method"; "shutdown" ]; [ "params"; [] ] ]
  |   ; message = "notification sent while connection is dead"
  |   ; kind = Code_error
  |   }
  | Raised at Dune_rpc_private.Client.Make.make_notification in file "otherlibs/dune-rpc/private/dune_rpc_private.ml", line 429, characters 8-36
  | Called from Fiber.Scheduler.exec in file "src/fiber/fiber.ml", line 878, characters 10-13
  \-----------------------------------------------------------------------
  $PATH/dune build --passive-watch-mode --root . received signal -7
  Error: exception Response.E
    { payload =
        Some
          [ [ "id"; [ [ "poll"; [ "auto"; "1" ] ]; [ "i"; "0" ] ] ]
          ; [ "req"
            ; [ [ "method"; "poll/diagnostic" ]
              ; [ "params"; [ "auto"; "1" ] ]
              ]
            ]
          ]
    ; message = "request sent while connection is dead"
    ; kind = Code_error
    }
  Raised at Dune_rpc_private.Client.Make.Stream.next in file
    "otherlibs/dune-rpc/private/dune_rpc_private.ml", line 487, characters
    10-36
  Called from Fiber.O.(>>|).(fun) in file "src/fiber/fiber.ml", line 255,
    characters 36-41
  Called from Fiber.Scheduler.exec in file "src/fiber/fiber.ml", line 878,
    characters 10-13
  Re-raised at Stdune__Exn.raise_with_backtrace in file
    "otherlibs/stdune/exn.ml", line 36, characters 27-56
  Called from Fiber.Scheduler.exec in file "src/fiber/fiber.ml", line 878,
    characters 10-13
  Error: exception Response.E
    { payload = Some [ [ "method"; "shutdown" ]; [ "params"; [] ] ]
    ; message = "notification sent while connection is dead"
    ; kind = Code_error
    }
  Raised at Dune_rpc_private.Client.Make.make_notification in file
    "otherlibs/dune-rpc/private/dune_rpc_private.ml", line 429, characters 8-36
  Called from Fiber.Scheduler.exec in file "src/fiber/fiber.ml", line 878,
    characters 10-13
  Re-raised at Stdune__Exn.raise_with_backtrace in file
    "otherlibs/stdune/exn.ml", line 36, characters 27-56
  Called from Fiber.Scheduler.exec in file "src/fiber/fiber.ml", line 878,
    characters 10-13 |}]

let%expect_test "optional promotion" =
  diagnostic_with_build
    [ ( "dune"
      , {|
(rule
 (alias foo)
 (action
  (progn
   (with-stdout-to output.expected (echo "foo"))
   (with-stdout-to output.actual (echo "bar"))
   (diff? output.expected output.actual))))
|}
      )
    ]
    "(alias foo)";
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Dune_util__Report_error.Already_reported)
  Raised at Dune_engine__Scheduler.Run.go.(fun) in file "src/dune_engine/scheduler.ml", line 1355, characters 27-40
  Called from Stdune__Exn.protectx in file "otherlibs/stdune/exn.ml", line 12, characters 8-11
  Re-raised at Stdune__Exn.protectx in file "otherlibs/stdune/exn.ml", line 18, characters 4-11
  Called from Dune_rpc_diagnostics.(fun) in file "test/expect-tests/dune_rpc_e2e/dune_rpc_diagnostics.ml", line 287, characters 2-261
  Called from Expect_test_collector.Make.Instance.exec in file "collector/expect_test_collector.ml", line 244, characters 12-19

  Trailing output
  ---------------

  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  Building (alias foo)
  Error building (alias foo):
  { payload = Some [ [ "id"; [ "auto"; "0" ] ] ]
  ; message =
      "connection terminated. this request will never receive a response"
  ; kind = Code_error
  }
  /-----------------------------------------------------------------------
  | Internal error: Uncaught exception.
  | Response.E
  |   { payload =
  |       Some
  |         [ [ "id"; [ [ "poll"; [ "auto"; "1" ] ]; [ "i"; "0" ] ] ]
  |         ; [ "req"
  |           ; [ [ "method"; "poll/diagnostic" ]
  |             ; [ "params"; [ "auto"; "1" ] ]
  |             ]
  |           ]
  |         ]
  |   ; message = "request sent while connection is dead"
  |   ; kind = Code_error
  |   }
  | Raised at Dune_rpc_private.Client.Make.Stream.next in file "otherlibs/dune-rpc/private/dune_rpc_private.ml", line 487, characters 10-36
  | Called from Fiber.O.(>>|).(fun) in file "src/fiber/fiber.ml", line 255, characters 36-41
  | Called from Fiber.Scheduler.exec in file "src/fiber/fiber.ml", line 878, characters 10-13
  \-----------------------------------------------------------------------
  /-----------------------------------------------------------------------
  | Internal error: Uncaught exception.
  | Response.E
  |   { payload = Some [ [ "method"; "shutdown" ]; [ "params"; [] ] ]
  |   ; message = "notification sent while connection is dead"
  |   ; kind = Code_error
  |   }
  | Raised at Dune_rpc_private.Client.Make.make_notification in file "otherlibs/dune-rpc/private/dune_rpc_private.ml", line 429, characters 8-36
  | Called from Fiber.Scheduler.exec in file "src/fiber/fiber.ml", line 878, characters 10-13
  \-----------------------------------------------------------------------
  $PATH/dune build --passive-watch-mode --root . received signal -7
  Error: exception Response.E
    { payload =
        Some
          [ [ "id"; [ [ "poll"; [ "auto"; "1" ] ]; [ "i"; "0" ] ] ]
          ; [ "req"
            ; [ [ "method"; "poll/diagnostic" ]
              ; [ "params"; [ "auto"; "1" ] ]
              ]
            ]
          ]
    ; message = "request sent while connection is dead"
    ; kind = Code_error
    }
  Raised at Dune_rpc_private.Client.Make.Stream.next in file
    "otherlibs/dune-rpc/private/dune_rpc_private.ml", line 487, characters
    10-36
  Called from Fiber.O.(>>|).(fun) in file "src/fiber/fiber.ml", line 255,
    characters 36-41
  Called from Fiber.Scheduler.exec in file "src/fiber/fiber.ml", line 878,
    characters 10-13
  Re-raised at Stdune__Exn.raise_with_backtrace in file
    "otherlibs/stdune/exn.ml", line 36, characters 27-56
  Called from Fiber.Scheduler.exec in file "src/fiber/fiber.ml", line 878,
    characters 10-13
  Error: exception Response.E
    { payload = Some [ [ "method"; "shutdown" ]; [ "params"; [] ] ]
    ; message = "notification sent while connection is dead"
    ; kind = Code_error
    }
  Raised at Dune_rpc_private.Client.Make.make_notification in file
    "otherlibs/dune-rpc/private/dune_rpc_private.ml", line 429, characters 8-36
  Called from Fiber.Scheduler.exec in file "src/fiber/fiber.ml", line 878,
    characters 10-13
  Re-raised at Stdune__Exn.raise_with_backtrace in file
    "otherlibs/stdune/exn.ml", line 36, characters 27-56
  Called from Fiber.Scheduler.exec in file "src/fiber/fiber.ml", line 878,
    characters 10-13 |}]

let%expect_test "warning detection" =
  diagnostic_with_build
    [ ("dune", "(executable (flags -w +26) (name foo))")
    ; ("foo.ml", "let () = let x = 10 in ()")
    ]
    "./foo.exe";
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Dune_util__Report_error.Already_reported)
  Raised at Dune_engine__Scheduler.Run.go.(fun) in file "src/dune_engine/scheduler.ml", line 1355, characters 27-40
  Called from Stdune__Exn.protectx in file "otherlibs/stdune/exn.ml", line 12, characters 8-11
  Re-raised at Stdune__Exn.protectx in file "otherlibs/stdune/exn.ml", line 18, characters 4-11
  Called from Dune_rpc_diagnostics.(fun) in file "test/expect-tests/dune_rpc_e2e/dune_rpc_diagnostics.ml", line 349, characters 2-148
  Called from Expect_test_collector.Make.Instance.exec in file "collector/expect_test_collector.ml", line 244, characters 12-19

  Trailing output
  ---------------

  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  Building ./foo.exe
  Error building ./foo.exe:
  { payload = Some [ [ "id"; [ "auto"; "0" ] ] ]
  ; message =
      "connection terminated. this request will never receive a response"
  ; kind = Code_error
  }
  /-----------------------------------------------------------------------
  | Internal error: Uncaught exception.
  | Response.E
  |   { payload =
  |       Some
  |         [ [ "id"; [ [ "poll"; [ "auto"; "1" ] ]; [ "i"; "0" ] ] ]
  |         ; [ "req"
  |           ; [ [ "method"; "poll/diagnostic" ]
  |             ; [ "params"; [ "auto"; "1" ] ]
  |             ]
  |           ]
  |         ]
  |   ; message = "request sent while connection is dead"
  |   ; kind = Code_error
  |   }
  | Raised at Dune_rpc_private.Client.Make.Stream.next in file "otherlibs/dune-rpc/private/dune_rpc_private.ml", line 487, characters 10-36
  | Called from Fiber.O.(>>|).(fun) in file "src/fiber/fiber.ml", line 255, characters 36-41
  | Called from Fiber.Scheduler.exec in file "src/fiber/fiber.ml", line 878, characters 10-13
  \-----------------------------------------------------------------------
  /-----------------------------------------------------------------------
  | Internal error: Uncaught exception.
  | Response.E
  |   { payload = Some [ [ "method"; "shutdown" ]; [ "params"; [] ] ]
  |   ; message = "notification sent while connection is dead"
  |   ; kind = Code_error
  |   }
  | Raised at Dune_rpc_private.Client.Make.make_notification in file "otherlibs/dune-rpc/private/dune_rpc_private.ml", line 429, characters 8-36
  | Called from Fiber.Scheduler.exec in file "src/fiber/fiber.ml", line 878, characters 10-13
  \-----------------------------------------------------------------------
  $PATH/dune build --passive-watch-mode --root . received signal -7
  Error: exception Response.E
    { payload =
        Some
          [ [ "id"; [ [ "poll"; [ "auto"; "1" ] ]; [ "i"; "0" ] ] ]
          ; [ "req"
            ; [ [ "method"; "poll/diagnostic" ]
              ; [ "params"; [ "auto"; "1" ] ]
              ]
            ]
          ]
    ; message = "request sent while connection is dead"
    ; kind = Code_error
    }
  Raised at Dune_rpc_private.Client.Make.Stream.next in file
    "otherlibs/dune-rpc/private/dune_rpc_private.ml", line 487, characters
    10-36
  Called from Fiber.O.(>>|).(fun) in file "src/fiber/fiber.ml", line 255,
    characters 36-41
  Called from Fiber.Scheduler.exec in file "src/fiber/fiber.ml", line 878,
    characters 10-13
  Re-raised at Stdune__Exn.raise_with_backtrace in file
    "otherlibs/stdune/exn.ml", line 36, characters 27-56
  Called from Fiber.Scheduler.exec in file "src/fiber/fiber.ml", line 878,
    characters 10-13
  Error: exception Response.E
    { payload = Some [ [ "method"; "shutdown" ]; [ "params"; [] ] ]
    ; message = "notification sent while connection is dead"
    ; kind = Code_error
    }
  Raised at Dune_rpc_private.Client.Make.make_notification in file
    "otherlibs/dune-rpc/private/dune_rpc_private.ml", line 429, characters 8-36
  Called from Fiber.Scheduler.exec in file "src/fiber/fiber.ml", line 878,
    characters 10-13
  Re-raised at Stdune__Exn.raise_with_backtrace in file
    "otherlibs/stdune/exn.ml", line 36, characters 27-56
  Called from Fiber.Scheduler.exec in file "src/fiber/fiber.ml", line 878,
    characters 10-13 |}]

let%expect_test "error from user rule" =
  diagnostic_with_build
    [ ("dune", "(rule (target foo) (action (bash \"echo foobar\")))") ]
    "./foo";
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Dune_util__Report_error.Already_reported)
  Raised at Dune_engine__Scheduler.Run.go.(fun) in file "src/dune_engine/scheduler.ml", line 1355, characters 27-40
  Called from Stdune__Exn.protectx in file "otherlibs/stdune/exn.ml", line 12, characters 8-11
  Re-raised at Stdune__Exn.protectx in file "otherlibs/stdune/exn.ml", line 18, characters 4-11
  Called from Dune_rpc_diagnostics.(fun) in file "test/expect-tests/dune_rpc_e2e/dune_rpc_diagnostics.ml", line 367, characters 2-107
  Called from Expect_test_collector.Make.Instance.exec in file "collector/expect_test_collector.ml", line 244, characters 12-19

  Trailing output
  ---------------

  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  Building ./foo
  Error building ./foo:
  { payload = Some [ [ "id"; [ "auto"; "0" ] ] ]
  ; message =
      "connection terminated. this request will never receive a response"
  ; kind = Code_error
  }
  /-----------------------------------------------------------------------
  | Internal error: Uncaught exception.
  | Response.E
  |   { payload =
  |       Some
  |         [ [ "id"; [ [ "poll"; [ "auto"; "1" ] ]; [ "i"; "0" ] ] ]
  |         ; [ "req"
  |           ; [ [ "method"; "poll/diagnostic" ]
  |             ; [ "params"; [ "auto"; "1" ] ]
  |             ]
  |           ]
  |         ]
  |   ; message = "request sent while connection is dead"
  |   ; kind = Code_error
  |   }
  | Raised at Dune_rpc_private.Client.Make.Stream.next in file "otherlibs/dune-rpc/private/dune_rpc_private.ml", line 487, characters 10-36
  | Called from Fiber.O.(>>|).(fun) in file "src/fiber/fiber.ml", line 255, characters 36-41
  | Called from Fiber.Scheduler.exec in file "src/fiber/fiber.ml", line 878, characters 10-13
  \-----------------------------------------------------------------------
  /-----------------------------------------------------------------------
  | Internal error: Uncaught exception.
  | Response.E
  |   { payload = Some [ [ "method"; "shutdown" ]; [ "params"; [] ] ]
  |   ; message = "notification sent while connection is dead"
  |   ; kind = Code_error
  |   }
  | Raised at Dune_rpc_private.Client.Make.make_notification in file "otherlibs/dune-rpc/private/dune_rpc_private.ml", line 429, characters 8-36
  | Called from Fiber.Scheduler.exec in file "src/fiber/fiber.ml", line 878, characters 10-13
  \-----------------------------------------------------------------------
  $PATH/dune build --passive-watch-mode --root . received signal -7
  Error: exception Response.E
    { payload =
        Some
          [ [ "id"; [ [ "poll"; [ "auto"; "1" ] ]; [ "i"; "0" ] ] ]
          ; [ "req"
            ; [ [ "method"; "poll/diagnostic" ]
              ; [ "params"; [ "auto"; "1" ] ]
              ]
            ]
          ]
    ; message = "request sent while connection is dead"
    ; kind = Code_error
    }
  Raised at Dune_rpc_private.Client.Make.Stream.next in file
    "otherlibs/dune-rpc/private/dune_rpc_private.ml", line 487, characters
    10-36
  Called from Fiber.O.(>>|).(fun) in file "src/fiber/fiber.ml", line 255,
    characters 36-41
  Called from Fiber.Scheduler.exec in file "src/fiber/fiber.ml", line 878,
    characters 10-13
  Re-raised at Stdune__Exn.raise_with_backtrace in file
    "otherlibs/stdune/exn.ml", line 36, characters 27-56
  Called from Fiber.Scheduler.exec in file "src/fiber/fiber.ml", line 878,
    characters 10-13
  Error: exception Response.E
    { payload = Some [ [ "method"; "shutdown" ]; [ "params"; [] ] ]
    ; message = "notification sent while connection is dead"
    ; kind = Code_error
    }
  Raised at Dune_rpc_private.Client.Make.make_notification in file
    "otherlibs/dune-rpc/private/dune_rpc_private.ml", line 429, characters 8-36
  Called from Fiber.Scheduler.exec in file "src/fiber/fiber.ml", line 878,
    characters 10-13
  Re-raised at Stdune__Exn.raise_with_backtrace in file
    "otherlibs/stdune/exn.ml", line 36, characters 27-56
  Called from Fiber.Scheduler.exec in file "src/fiber/fiber.ml", line 878,
    characters 10-13 |}]

let%expect_test "create and fix error" =
  setup_diagnostics (fun client ->
      files
        [ ("dune", "(executable (name foo))")
        ; ("foo.ml", "let () = print_endline 123")
        ];
      let* poll = poll_exn client Dune_rpc.Public.Sub.diagnostic in
      let* () = dune_build client "./foo.exe" in
      [%expect {|
        Building ./foo.exe
        Error building ./foo.exe:
        { payload = Some [ [ "id"; [ "auto"; "1" ] ] ]
        ; message =
            "connection terminated. this request will never receive a response"
        ; kind = Code_error
        } |}];
      let* () = print_diagnostics poll in
      [%expect.unreachable];
      files [ ("foo.ml", "let () = print_endline \"foo\"") ];
      let* () = dune_build client "./foo.exe" in
      [%expect.unreachable];
      let+ () = print_diagnostics poll in
      [%expect.unreachable]);
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Dune_util__Report_error.Already_reported)
  Raised at Dune_engine__Scheduler.Run.go.(fun) in file "src/dune_engine/scheduler.ml", line 1355, characters 27-40
  Called from Stdune__Exn.protectx in file "otherlibs/stdune/exn.ml", line 12, characters 8-11
  Re-raised at Stdune__Exn.protectx in file "otherlibs/stdune/exn.ml", line 18, characters 4-11
  Called from Dune_rpc_diagnostics.(fun) in file "test/expect-tests/dune_rpc_e2e/dune_rpc_diagnostics.ml", line 415, characters 2-1023
  Called from Expect_test_collector.Make.Instance.exec in file "collector/expect_test_collector.ml", line 244, characters 12-19

  Trailing output
  ---------------

  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  /-----------------------------------------------------------------------
  | Internal error: Uncaught exception.
  | Response.E
  |   { payload =
  |       Some
  |         [ [ "id"; [ [ "poll"; [ "auto"; "0" ] ]; [ "i"; "0" ] ] ]
  |         ; [ "req"
  |           ; [ [ "method"; "poll/diagnostic" ]
  |             ; [ "params"; [ "auto"; "0" ] ]
  |             ]
  |           ]
  |         ]
  |   ; message = "request sent while connection is dead"
  |   ; kind = Code_error
  |   }
  | Raised at Dune_rpc_private.Client.Make.Stream.next in file "otherlibs/dune-rpc/private/dune_rpc_private.ml", line 487, characters 10-36
  | Called from Fiber.O.(>>|).(fun) in file "src/fiber/fiber.ml", line 255, characters 36-41
  | Called from Fiber.Scheduler.exec in file "src/fiber/fiber.ml", line 878, characters 10-13
  \-----------------------------------------------------------------------
  /-----------------------------------------------------------------------
  | Internal error: Uncaught exception.
  | Response.E
  |   { payload = Some [ [ "method"; "shutdown" ]; [ "params"; [] ] ]
  |   ; message = "notification sent while connection is dead"
  |   ; kind = Code_error
  |   }
  | Raised at Dune_rpc_private.Client.Make.make_notification in file "otherlibs/dune-rpc/private/dune_rpc_private.ml", line 429, characters 8-36
  | Called from Fiber.Scheduler.exec in file "src/fiber/fiber.ml", line 878, characters 10-13
  \-----------------------------------------------------------------------
  $PATH/dune build --passive-watch-mode --root . received signal -7
  Error: exception Response.E
    { payload =
        Some
          [ [ "id"; [ [ "poll"; [ "auto"; "0" ] ]; [ "i"; "0" ] ] ]
          ; [ "req"
            ; [ [ "method"; "poll/diagnostic" ]
              ; [ "params"; [ "auto"; "0" ] ]
              ]
            ]
          ]
    ; message = "request sent while connection is dead"
    ; kind = Code_error
    }
  Raised at Dune_rpc_private.Client.Make.Stream.next in file
    "otherlibs/dune-rpc/private/dune_rpc_private.ml", line 487, characters
    10-36
  Called from Fiber.O.(>>|).(fun) in file "src/fiber/fiber.ml", line 255,
    characters 36-41
  Called from Fiber.Scheduler.exec in file "src/fiber/fiber.ml", line 878,
    characters 10-13
  Re-raised at Stdune__Exn.raise_with_backtrace in file
    "otherlibs/stdune/exn.ml", line 36, characters 27-56
  Called from Fiber.Scheduler.exec in file "src/fiber/fiber.ml", line 878,
    characters 10-13
  Error: exception Response.E
    { payload = Some [ [ "method"; "shutdown" ]; [ "params"; [] ] ]
    ; message = "notification sent while connection is dead"
    ; kind = Code_error
    }
  Raised at Dune_rpc_private.Client.Make.make_notification in file
    "otherlibs/dune-rpc/private/dune_rpc_private.ml", line 429, characters 8-36
  Called from Fiber.Scheduler.exec in file "src/fiber/fiber.ml", line 878,
    characters 10-13
  Re-raised at Stdune__Exn.raise_with_backtrace in file
    "otherlibs/stdune/exn.ml", line 36, characters 27-56
  Called from Fiber.Scheduler.exec in file "src/fiber/fiber.ml", line 878,
    characters 10-13 |}]

let request_exn client req n =
  let* staged = Client.Versioned.prepare_request client req in
  match staged with
  | Ok req -> Client.request client req n
  | Error e -> raise (Dune_rpc.Version_error.E e)

let%expect_test "formatting dune files" =
  let exec _pid =
    run_client (fun client ->
        (* First we test for regular errors *)
        files [ ("dune-project", "(lang dune 3.0)") ];
        let unformatted = "(\nlibrary (name foo\n))" in
        printfn "Unformatted:\n%s" unformatted;
        let run uri what =
          let+ res =
            request_exn client Request.format_dune_file
              (uri, `Contents unformatted)
          in
          match res with
          | Ok s -> printfn "Formatted (%s):\n%s" what s
          | Error e ->
            Format.eprintf "Error formatting:@.%s@."
              (Dyn.to_string (Response.Error.to_dyn e))
        in
        let* () = run Dune_rpc.Path.(relative dune_root "dune") "relative" in
        [%expect
          {|
          Unformatted:
          (
          library (name foo
          ))
          Formatted (relative):
          (library
           (name foo)) |}];
        let+ () =
          run
            (Dune_rpc.Path.absolute (Filename.concat (Sys.getcwd ()) "dune"))
            "absolute"
        in
        [%expect
          {|
          Formatted (absolute):
          (library
           (name foo)) |}])
  in
  run (fun () -> with_dune_watch exec);
  [%expect {| |}]

let%expect_test "promoting dune files" =
  let exec _pid =
    run_client (fun client ->
        (* First we test for regular errors *)
        let fname = "x" in
        let promoted = "x.gen" in
        files
          [ ("dune-project", "(lang dune 3.0)")
          ; ("x", "titi")
          ; ( "dune"
            , sprintf
                {|
(rule (alias foo) (action (diff %s %s)))
(rule (with-stdout-to %s (echo "toto")))
|}
                fname promoted promoted )
          ];
        let* () = dune_build client "(alias foo)" in
        [%expect
          {|
          Building (alias foo)
          Error building (alias foo):
          { payload = Some [ [ "id"; [ "auto"; "0" ] ] ]
          ; message =
              "connection terminated. this request will never receive a response"
          ; kind = Code_error
          } |}];
        print_endline "attempting to promote";
        let+ res =
          request_exn client Request.promote
            Dune_rpc.Path.(relative dune_root fname)
        in
        (match res with
        | Ok () ->
          let contents = Io.String_path.read_file fname in
          printfn "promoted file contents:\n%s" contents
        | Error e ->
          Format.eprintf "Error formatting:@.%s@."
            (Dyn.to_string (Dune_rpc.Response.Error.to_dyn e)));
        [%expect
          {|
          attempting to promote
          Error formatting:
          { payload =
              Some
                [ [ "id"; [ "auto"; "1" ] ]
                ; [ "req"; [ [ "method"; "promote" ]; [ "params"; "./x" ] ] ]
                ]
          ; message = "request sent while connection is dead"
          ; kind = Code_error
          } |}])
  in
  run (fun () -> with_dune_watch exec);
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Dune_util__Report_error.Already_reported)
  Raised at Dune_engine__Scheduler.Run.go.(fun) in file "src/dune_engine/scheduler.ml", line 1355, characters 27-40
  Called from Stdune__Exn.protectx in file "otherlibs/stdune/exn.ml", line 12, characters 8-11
  Re-raised at Stdune__Exn.protectx in file "otherlibs/stdune/exn.ml", line 18, characters 4-11
  Called from Dune_rpc_diagnostics.(fun) in file "test/expect-tests/dune_rpc_e2e/dune_rpc_diagnostics.ml", line 602, characters 2-38
  Called from Expect_test_collector.Make.Instance.exec in file "collector/expect_test_collector.ml", line 244, characters 12-19

  Trailing output
  ---------------

  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  /-----------------------------------------------------------------------
  | Internal error: Uncaught exception.
  | Response.E
  |   { payload = Some [ [ "method"; "shutdown" ]; [ "params"; [] ] ]
  |   ; message = "notification sent while connection is dead"
  |   ; kind = Code_error
  |   }
  | Raised at Dune_rpc_private.Client.Make.make_notification in file "otherlibs/dune-rpc/private/dune_rpc_private.ml", line 429, characters 8-36
  | Called from Fiber.Scheduler.exec in file "src/fiber/fiber.ml", line 878, characters 10-13
  | Re-raised at Stdune__Exn.raise_with_backtrace in file "otherlibs/stdune/exn.ml", line 36, characters 27-56
  | Called from Fiber.Scheduler.exec in file "src/fiber/fiber.ml", line 878, characters 10-13
  | Re-raised at Stdune__Exn.raise_with_backtrace in file "otherlibs/stdune/exn.ml", line 36, characters 27-56
  | Called from Fiber.Scheduler.exec in file "src/fiber/fiber.ml", line 878, characters 10-13
  \-----------------------------------------------------------------------
  $PATH/dune build --passive-watch-mode --root . received signal -7
  Error: exception Response.E
    { payload = Some [ [ "method"; "shutdown" ]; [ "params"; [] ] ]
    ; message = "notification sent while connection is dead"
    ; kind = Code_error
    }
  Raised at Dune_rpc_private.Client.Make.make_notification in file
    "otherlibs/dune-rpc/private/dune_rpc_private.ml", line 429, characters 8-36
  Called from Fiber.Scheduler.exec in file "src/fiber/fiber.ml", line 878,
    characters 10-13
  Re-raised at Stdune__Exn.raise_with_backtrace in file
    "otherlibs/stdune/exn.ml", line 36, characters 27-56
  Called from Fiber.Scheduler.exec in file "src/fiber/fiber.ml", line 878,
    characters 10-13
  Re-raised at Stdune__Exn.raise_with_backtrace in file
    "otherlibs/stdune/exn.ml", line 36, characters 27-56
  Called from Fiber.Scheduler.exec in file "src/fiber/fiber.ml", line 878,
    characters 10-13
  Re-raised at Stdune__Exn.raise_with_backtrace in file
    "otherlibs/stdune/exn.ml", line 36, characters 27-56
  Called from Fiber.Scheduler.exec in file "src/fiber/fiber.ml", line 878,
    characters 10-13
  Re-raised at Stdune__Exn.raise_with_backtrace in file
    "otherlibs/stdune/exn.ml", line 36, characters 27-56
  Called from Fiber.Scheduler.exec in file "src/fiber/fiber.ml", line 878,
    characters 10-13 |}]
