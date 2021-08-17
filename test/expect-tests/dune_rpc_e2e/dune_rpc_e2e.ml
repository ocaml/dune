open Stdune
open Fiber.O
module Scheduler = Dune_engine.Scheduler
module Dune_rpc = Dune_rpc_private
module Request = Dune_rpc.Public.Request
module Diagnostic = Dune_rpc.Diagnostic
module Client = Dune_rpc_impl.Client
module Session = Csexp_rpc.Session
module Config = Dune_util.Config

let () = Dune_util.Log.init ~file:(Out_channel stderr) ()

let dune_prog =
  lazy
    (let path = Env.path Env.initial in
     Bin.which ~path "dune" |> Option.value_exn |> Path.to_absolute_filename)

let init_chan ~root_dir =
  let build_dir = Filename.concat root_dir "_build" in
  let once () =
    match Dune_rpc_impl.Where.Where.get ~build_dir with
    | None -> Fiber.return None
    | Some where -> (
      let* client = Dune_rpc_impl.Run.Connect.csexp_client where in
      let+ res = Csexp_rpc.Client.connect client in
      match res with
      | Ok s -> Some s
      | Error _ -> None)
  in
  let rec loop () =
    let* res = once () in
    match res with
    | Some res -> Fiber.return res
    | None -> Scheduler.sleep 0.2 >>= loop
  in
  loop ()

let run_client ?handler f =
  let* chan = init_chan ~root_dir:"." in
  let initialize =
    let id = Dune_rpc.Id.make (Atom "test") in
    Dune_rpc.Initialize.Request.create ~id
  in
  Client.connect ?handler chan initialize ~f:(fun client ->
      Fiber.finalize
        (fun () -> f client)
        ~finally:
          (Client.notification client Dune_rpc.Public.Notification.shutdown))

let read_lines in_ =
  let* reader = Scheduler.Worker.create () in
  let in_ = Unix.in_channel_of_descr in_ in
  let rec loop acc =
    let* res = Scheduler.Worker.task reader ~f:(fun () -> input_line in_) in
    match res with
    | Ok a -> loop (a :: acc)
    | Error `Stopped -> assert false
    | Error (`Exn e) ->
      (match e.exn with
      | End_of_file -> ()
      | _ ->
        Format.eprintf "Error reading channel: %a@.%!"
          Exn_with_backtrace.pp_uncaught e);
      Fiber.return (String.concat (List.rev acc) ~sep:"\n")
  in
  let+ res = loop [] in
  Scheduler.Worker.stop reader;
  close_in_noerr in_;
  res

let run ~prog ~argv =
  let stdout_i, stdout_w = Unix.pipe ~cloexec:true () in
  let stderr_i, stderr_w = Unix.pipe ~cloexec:true () in
  let pid =
    let argv = prog :: argv in
    Spawn.spawn ~prog ~argv ~stdout:stdout_w ~stderr:stderr_w
      ~stdin:(Lazy.force Config.dev_null_in)
      ()
    |> Pid.of_int
  in
  Unix.close stdout_w;
  Unix.close stderr_w;
  ( (let+ proc = Scheduler.wait_for_process ~timeout:3.0 pid in
     if proc.status <> Unix.WEXITED 0 then
       let name = sprintf "%s %s" prog (String.concat ~sep:" " argv) in
       match proc.status with
       | Unix.WEXITED i -> printfn "%s returned %d" name i
       | Unix.WSIGNALED i -> printfn "%s received signal %i" name i
       | _ -> assert false)
  , read_lines stdout_i
  , read_lines stderr_i )

let run_server ~root_dir =
  run ~prog:(Lazy.force dune_prog)
    ~argv:[ "build"; "--passive-watch-mode"; "--root"; root_dir ]

let dune_build client what =
  printfn "Building %s" what;
  let+ res = Client.request client Dune_rpc_impl.Decl.build [ what ] in
  match res with
  | Error e ->
    Format.eprintf "Error building %s:@.%s@." what
      (Dyn.to_string (Dune_rpc.Response.Error.to_dyn e))
  | Ok res ->
    printfn "Build %s %s" what
      (match res with
      | Success -> "succeeded"
      | Failure -> "failed")

let test f =
  let root_dir = "." in
  let run_server, server_stdout, server_stderr = run_server ~root_dir in
  let+ stdout, stderr =
    Fiber.fork_and_join_unit
      (fun () -> Fiber.fork_and_join_unit (fun () -> run_server) f)
      (fun () ->
        Fiber.fork_and_join (fun () -> server_stdout) (fun () -> server_stderr))
  in
  (* We wait until the tests finish to print stdout and stderr for determinism.
     But this has the disadvantage that the fiber above will not always
     terminate for failed tests. Thus, the output below will never be shown. *)
  if stdout <> "" then printfn "stdout:\n%s" stdout;
  if stderr <> "" then printfn "stderr:\n%s" stderr

let run =
  let cwd = Sys.getcwd () in
  let config =
    { Scheduler.Config.concurrency = 1
    ; display = { verbosity = Quiet; status_line = false }
    ; rpc = None
    ; stats = None
    }
  in
  fun run ->
    let dir = Temp.create Dir ~prefix:"dune" ~suffix:"rpc_test" in
    let run () =
      Fiber.with_error_handler run ~on_error:(fun exn ->
          Exn_with_backtrace.pp_uncaught Format.err_formatter exn;
          Format.pp_print_flush Format.err_formatter ();
          Exn_with_backtrace.reraise exn)
    in
    Exn.protect
      ~finally:(fun () -> Sys.chdir cwd)
      ~f:(fun () ->
        Sys.chdir (Path.to_string dir);
        Scheduler.Run.go config run ~timeout:5.0 ~on_event:(fun _ _ -> ()))

let%expect_test "turn on and shutdown" =
  let test () =
    test (fun () ->
        run_client (fun client ->
            let+ () = dune_build client "." in
            printfn "shutting down"))
  in
  run test;
  [%expect
    {|
    Building .
    Build . succeeded
    shutting down
    stderr:
    waiting for inotify sync
    waited for inotify sync
    Success, waiting for filesystem changes... |}]

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
  let sanitize (d : Dune_rpc.Diagnostic.t) =
    let directory = Option.map d.directory ~f:sanitize_path in
    let promotion =
      List.map d.promotion ~f:(fun (p : Dune_rpc.Diagnostic.Promotion.t) ->
          let in_build = sanitize_path p.in_build in
          let in_source = sanitize_path p.in_source in
          { Dune_rpc.Diagnostic.Promotion.in_build; in_source })
    in
    let related =
      List.map d.related ~f:(fun (related : Dune_rpc.Diagnostic.Related.t) ->
          let loc = sanitize_loc related.loc in
          let message = sanitize_pp related.message in
          { Dune_rpc.Diagnostic.Related.message; loc })
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
          Dune_rpc.Diagnostic.promotion e
          |> List.iter ~f:(fun promotion ->
                 let path = Dune_rpc.Diagnostic.Promotion.in_build promotion in
                 if not (Sys.file_exists path) then
                   printfn "FAILURE: promotion file %s does not exist"
                     (sanitize_path path)));
        let e = map_event e sanitize in
        printfn "%s" (Dyn.to_string (Dune_rpc.Diagnostic.Event.to_dyn e)))

let setup_diagnostics f =
  let exec () =
    run_client (fun client ->
        (* First we test for regular errors *)
        files [ ("dune-project", "(lang dune 3.0)") ];
        f client)
  in
  run (fun () -> test exec)

let print_diagnostics poll =
  let+ res = Client.Stream.next poll in
  match res with
  | None -> printfn "client: no more diagnostics"
  | Some diag -> on_diagnostic_event diag

let diagnostic_with_build setup target =
  let exec () =
    run_client (fun client ->
        (* First we test for regular errors *)
        files (("dune-project", "(lang dune 3.0)") :: setup);
        let* () = dune_build client target in
        let poll = Client.poll client Dune_rpc.Sub.diagnostic in
        let* () = print_diagnostics poll in
        Client.Stream.cancel poll)
  in
  run (fun () -> test exec)

let%expect_test "error in dune file" =
  diagnostic_with_build [ ("dune", "(library (name foo))") ] "foo.cma";
  [%expect
    {|
    Building foo.cma
    Build foo.cma succeeded
    <no diagnostics>
    stderr:
    waiting for inotify sync
    waited for inotify sync
    Success, waiting for filesystem changes... |}]

let%expect_test "related error" =
  diagnostic_with_build
    [ ("dune", "(library (name foo))")
    ; ("foo.mli", "val x : int")
    ; ("foo.ml", "let x = true")
    ]
    "foo.cma";
  [%expect
    {|
    Building foo.cma
    Build foo.cma failed
    [ "Add"
    ; [ [ "directory"; "$CWD" ]
      ; [ "id"; "0" ]
      ; [ "loc"
        ; [ [ "start"
            ; [ [ "pos_bol"; "0" ]
              ; [ "pos_cnum"; "0" ]
              ; [ "pos_fname"; "$CWD/foo.ml" ]
              ; [ "pos_lnum"; "1" ]
              ]
            ]
          ; [ "stop"
            ; [ [ "pos_bol"; "0" ]
              ; [ "pos_cnum"; "0" ]
              ; [ "pos_fname"; "$CWD/foo.ml" ]
              ; [ "pos_lnum"; "1" ]
              ]
            ]
          ]
        ]
      ; [ "message"
        ; [ "Verbatim"
          ; "The implementation foo.ml\n\
            \       does not match the interface .foo.objs/byte/foo.cmi:\n\
            \       Values do not match: val x : bool is not included in val x : int\n\
             "
          ]
        ]
      ; [ "promotion"; [] ]
      ; [ "related"
        ; [ [ [ "loc"
              ; [ [ "start"
                  ; [ [ "pos_bol"; "0" ]
                    ; [ "pos_cnum"; "0" ]
                    ; [ "pos_fname"; "$CWD/foo.mli" ]
                    ; [ "pos_lnum"; "1" ]
                    ]
                  ]
                ; [ "stop"
                  ; [ [ "pos_bol"; "0" ]
                    ; [ "pos_cnum"; "11" ]
                    ; [ "pos_fname"; "$CWD/foo.mli" ]
                    ; [ "pos_lnum"; "1" ]
                    ]
                  ]
                ]
              ]
            ; [ "message"; [ "Verbatim"; "Expected declaration\n\
                                          " ] ]
            ]
          ; [ [ "loc"
              ; [ [ "start"
                  ; [ [ "pos_bol"; "0" ]
                    ; [ "pos_cnum"; "4" ]
                    ; [ "pos_fname"; "$CWD/foo.ml" ]
                    ; [ "pos_lnum"; "1" ]
                    ]
                  ]
                ; [ "stop"
                  ; [ [ "pos_bol"; "0" ]
                    ; [ "pos_cnum"; "5" ]
                    ; [ "pos_fname"; "$CWD/foo.ml" ]
                    ; [ "pos_lnum"; "1" ]
                    ]
                  ]
                ]
              ]
            ; [ "message"; [ "Verbatim"; "Actual declaration\n\
                                          \n\
                                          " ] ]
            ]
          ]
        ]
      ; [ "targets"; [] ]
      ]
    ]
    stderr:
    waiting for inotify sync
    waited for inotify sync
    File "foo.ml", line 1:
    Error: The implementation foo.ml
           does not match the interface .foo.objs/byte/foo.cmi:
           Values do not match: val x : bool is not included in val x : int
           File "foo.mli", line 1, characters 0-11: Expected declaration
           File "foo.ml", line 1, characters 4-5: Actual declaration
    Had errors, waiting for filesystem changes... |}]

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
  [%expect
    {|
    Building (alias foo)
    Build (alias foo) failed
    [ "Add"
    ; [ [ "id"; "0" ]
      ; [ "loc"
        ; [ [ "start"
            ; [ [ "pos_bol"; "0" ]
              ; [ "pos_cnum"; "0" ]
              ; [ "pos_fname"; "x" ]
              ; [ "pos_lnum"; "1" ]
              ]
            ]
          ; [ "stop"
            ; [ [ "pos_bol"; "0" ]
              ; [ "pos_cnum"; "0" ]
              ; [ "pos_fname"; "x" ]
              ; [ "pos_lnum"; "1" ]
              ]
            ]
          ]
        ]
      ; [ "message"
        ; [ "Verbatim"
          ; "Error: Files _build/default/x and _build/default/x.gen\n\
             differ.\n\
             "
          ]
        ]
      ; [ "promotion"
        ; [ [ [ "in_build"; "$CWD/_build/default/x.gen" ]
            ; [ "in_source"; "$CWD/x" ]
            ]
          ]
        ]
      ; [ "related"; [] ]
      ; [ "targets"; [] ]
      ]
    ]
    stderr:
    waiting for inotify sync
    waited for inotify sync
    File "x", line 1, characters 0-0:
    Error: Files _build/default/x and _build/default/x.gen differ.
    Had errors, waiting for filesystem changes... |}]

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
  [%expect
    {|
    Building (alias foo)
    Build (alias foo) failed
    FAILURE: promotion file $CWD/_build/default/output.actual does not exist
    [ "Add"
    ; [ [ "id"; "0" ]
      ; [ "loc"
        ; [ [ "start"
            ; [ [ "pos_bol"; "0" ]
              ; [ "pos_cnum"; "0" ]
              ; [ "pos_fname"; "output.expected" ]
              ; [ "pos_lnum"; "1" ]
              ]
            ]
          ; [ "stop"
            ; [ [ "pos_bol"; "0" ]
              ; [ "pos_cnum"; "0" ]
              ; [ "pos_fname"; "output.expected" ]
              ; [ "pos_lnum"; "1" ]
              ]
            ]
          ]
        ]
      ; [ "message"
        ; [ "Verbatim"
          ; "Error: Files _build/default/output.expected and _build/default/output.actual\n\
             differ.\n\
             "
          ]
        ]
      ; [ "promotion"
        ; [ [ [ "in_build"; "$CWD/_build/default/output.actual" ]
            ; [ "in_source"; "$CWD/output.expected" ]
            ]
          ]
        ]
      ; [ "related"; [] ]
      ; [ "targets"; [] ]
      ]
    ]
    stderr:
    waiting for inotify sync
    waited for inotify sync
    File "output.expected", line 1, characters 0-0:
    Error: Files _build/default/output.expected and _build/default/output.actual
    differ.
    Had errors, waiting for filesystem changes... |}]

let%expect_test "warning detection" =
  diagnostic_with_build
    [ ("dune", "(executable (flags -w +26) (name foo))")
    ; ("foo.ml", "let () = let x = 10 in ()")
    ]
    "./foo.exe";
  [%expect
    {|
    Building ./foo.exe
    Build ./foo.exe succeeded
    <no diagnostics>
    stderr:
    waiting for inotify sync
    waited for inotify sync
    File "foo.ml", line 1, characters 13-14:
    1 | let () = let x = 10 in ()
                     ^
    Warning 26 [unused-var]: unused variable x.
    Success, waiting for filesystem changes... |}]

let%expect_test "error from user rule" =
  diagnostic_with_build
    [ ("dune", "(rule (target foo) (action (bash \"echo foobar\")))") ]
    "./foo";
  [%expect
    {|
    Building ./foo
    Build ./foo failed
    [ "Add"
    ; [ [ "id"; "0" ]
      ; [ "loc"
        ; [ [ "start"
            ; [ [ "pos_bol"; "0" ]
              ; [ "pos_cnum"; "0" ]
              ; [ "pos_fname"; "dune" ]
              ; [ "pos_lnum"; "1" ]
              ]
            ]
          ; [ "stop"
            ; [ [ "pos_bol"; "0" ]
              ; [ "pos_cnum"; "49" ]
              ; [ "pos_fname"; "dune" ]
              ; [ "pos_lnum"; "1" ]
              ]
            ]
          ]
        ]
      ; [ "message"
        ; [ "Verbatim"
          ; "Error: Rule failed to generate the following\n\
             targets:- foo\n\
             "
          ]
        ]
      ; [ "promotion"; [] ]
      ; [ "related"; [] ]
      ; [ "targets"; [] ]
      ]
    ]
    stderr:
    waiting for inotify sync
    waited for inotify sync
            bash foo
    foobar
    File "dune", line 1, characters 0-49:
    1 | (rule (target foo) (action (bash "echo foobar")))
        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    Error: Rule failed to generate the following targets:
    - foo
    Had errors, waiting for filesystem changes... |}]

let%expect_test "create and fix error" =
  setup_diagnostics (fun client ->
      files
        [ ("dune", "(executable (name foo))")
        ; ("foo.ml", "let () = print_endline 123")
        ];
      let poll = Client.poll client Dune_rpc.Sub.diagnostic in
      let* () = dune_build client "./foo.exe" in
      [%expect {|
        Building ./foo.exe
        Build ./foo.exe failed |}];
      let* () = print_diagnostics poll in
      [%expect
        {|
        [ "Add"
        ; [ [ "directory"; "$CWD" ]
          ; [ "id"; "0" ]
          ; [ "loc"
            ; [ [ "start"
                ; [ [ "pos_bol"; "0" ]
                  ; [ "pos_cnum"; "23" ]
                  ; [ "pos_fname"; "$CWD/foo.ml" ]
                  ; [ "pos_lnum"; "1" ]
                  ]
                ]
              ; [ "stop"
                ; [ [ "pos_bol"; "0" ]
                  ; [ "pos_cnum"; "26" ]
                  ; [ "pos_fname"; "$CWD/foo.ml" ]
                  ; [ "pos_lnum"; "1" ]
                  ]
                ]
              ]
            ]
          ; [ "message"
            ; [ "Verbatim"
              ; "This expression has type int but an expression was expected of type\n\
                \         string\n\
                 \n\
                 "
              ]
            ]
          ; [ "promotion"; [] ]
          ; [ "related"; [] ]
          ; [ "targets"; [] ]
          ]
        ] |}];
      files [ ("foo.ml", "let () = print_endline \"foo\"") ];
      let* () = dune_build client "./foo.exe" in
      [%expect
        {|
        Building ./foo.exe
        Build ./foo.exe succeeded |}];
      let+ () = print_diagnostics poll in
      [%expect
        {|
        [ "Remove"
        ; [ [ "directory"; "$CWD" ]
          ; [ "id"; "0" ]
          ; [ "loc"
            ; [ [ "start"
                ; [ [ "pos_bol"; "0" ]
                  ; [ "pos_cnum"; "23" ]
                  ; [ "pos_fname"; "$CWD/foo.ml" ]
                  ; [ "pos_lnum"; "1" ]
                  ]
                ]
              ; [ "stop"
                ; [ [ "pos_bol"; "0" ]
                  ; [ "pos_cnum"; "26" ]
                  ; [ "pos_fname"; "$CWD/foo.ml" ]
                  ; [ "pos_lnum"; "1" ]
                  ]
                ]
              ]
            ]
          ; [ "message"
            ; [ "Verbatim"
              ; "This expression has type int but an expression was expected of type\n\
                \         string\n\
                 \n\
                 "
              ]
            ]
          ; [ "promotion"; [] ]
          ; [ "related"; [] ]
          ; [ "targets"; [] ]
          ]
        ] |}]);
  [%expect
    {|
    stderr:
    waiting for inotify sync
    waited for inotify sync
    File "foo.ml", line 1, characters 23-26:
    1 | let () = print_endline 123
                               ^^^
    Error: This expression has type int but an expression was expected of type
             string
    Had errors, waiting for filesystem changes...
    waiting for inotify sync
    waited for inotify sync
    Success, waiting for filesystem changes... |}]

let%expect_test "formatting dune files" =
  let exec () =
    run_client (fun client ->
        (* First we test for regular errors *)
        files [ ("dune-project", "(lang dune 3.0)") ];
        let unformatted = "(\nlibrary (name foo\n))" in
        printfn "Unformatted:\n%s" unformatted;
        let run uri what =
          let+ res =
            Client.request client Request.format_dune_file
              (uri, `Contents unformatted)
          in
          match res with
          | Ok s -> printfn "Formatted (%s):\n%s" what s
          | Error e ->
            Format.eprintf "Error formatting:@.%s@."
              (Dyn.to_string (Dune_rpc.Response.Error.to_dyn e))
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
  run (fun () -> test exec);
  [%expect {| |}]

let%expect_test "promoting dune files" =
  let exec () =
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
          Build (alias foo) failed |}];
        print_endline "attempting to promote";
        let+ res =
          Client.request client Request.promote
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
          promoted file contents:
          toto |}])
  in
  run (fun () -> test exec);
  [%expect
    {|
    stderr:
    waiting for inotify sync
    waited for inotify sync
    File "x", line 1, characters 0-0:
    Error: Files _build/default/x and _build/default/x.gen differ.
    Had errors, waiting for filesystem changes...
    Promoting _build/default/x.gen to x. |}]
