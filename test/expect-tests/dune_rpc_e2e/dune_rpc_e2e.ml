open Stdune
open Fiber.O
module Scheduler = Dune_engine.Scheduler
module Dune_rpc = Dune_rpc_private
module Client = Dune_rpc_impl.Client
module Session = Csexp_rpc.Session
module Config = Dune_util.Config

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
  in
  Unix.close stdout_w;
  Unix.close stderr_w;
  ( pid
  , (let+ proc = Scheduler.wait_for_process ~timeout:3.0 (Pid.of_int pid) in
     if proc.status <> Unix.WEXITED 0 then
       let name = sprintf "%s %s" prog (String.concat ~sep:" " argv) in
       match proc.status with
       | Unix.WEXITED i -> printfn "%s returned %d" name i
       | Unix.WSIGNALED i -> printfn "%s received signal %i" name i
       | _ -> assert false)
  , read_lines stdout_i
  , read_lines stderr_i )

let run_dump_out ~prog ~argv =
  let _pid, finish, stdout, stderr = run ~prog ~argv in
  let me =
    sprintf "%s %s" (Filename.basename prog) (String.concat argv ~sep:" ")
  in
  let print what =
    let+ what = what in
    let what = String.trim what in
    if what <> "" then printfn "%s > %s" me what
  in
  Fiber.fork_and_join_unit
    (fun () -> finish)
    (fun () ->
      Fiber.fork_and_join_unit (fun () -> print stdout) (fun () -> print stderr))

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
  let _server_pid, run_server, server_stdout, server_stderr =
    run_server ~root_dir
  in
  let+ stdout, stderr =
    Fiber.fork_and_join_unit
      (fun () -> Fiber.fork_and_join_unit (fun () -> run_server) f)
      (fun () ->
        Fiber.fork_and_join (fun () -> server_stdout) (fun () -> server_stderr))
  in
  if stdout <> "" then printfn "stdout:\n%s" stdout;
  if stderr <> "" then printfn "stderr:\n%s" stderr

let cwd = Sys.getcwd ()

let run =
  let config =
    { Scheduler.Config.concurrency = 1
    ; display = { verbosity = Quiet; status_line = false }
    ; rpc = None
    ; stats = None
    }
  in
  fun run ->
    let dir = Temp.create Dir ~prefix:"dune" ~suffix:"rpc_test" in
    Exn.protect
      ~finally:(fun () -> Sys.chdir cwd)
      ~f:(fun () ->
        Sys.chdir (Path.to_string dir);
        Scheduler.Run.go config run ~on_event:(fun _ _ -> ()))

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

let setup_diagnostics f =
  let exec () =
    let handler =
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
      let sanitize (d : Dune_rpc.Diagnostic.t) =
        let directory = Option.map d.directory ~f:sanitize_path in
        let promotion =
          List.map d.promotion ~f:(fun (p : Dune_rpc.Diagnostic.Promotion.t) ->
              let in_build = sanitize_path p.in_build in
              let in_source = sanitize_path p.in_source in
              { Dune_rpc.Diagnostic.Promotion.in_build; in_source })
        in
        let related =
          List.map d.related
            ~f:(fun (related : Dune_rpc.Diagnostic.Related.t) ->
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
      let on_diagnostic_event (e : Dune_rpc.Diagnostic.Event.t) =
        (match e with
        | Remove _ -> ()
        | Add e ->
          Dune_rpc.Diagnostic.promotion e
          |> List.iter ~f:(fun promotion ->
                 let path = Dune_rpc.Diagnostic.Promotion.in_build promotion in
                 if not (Sys.file_exists path) then
                   printfn "FAILURE: promotion file %s does not exist"
                     (sanitize_path path)));
        let e =
          match e with
          | Add e -> Dune_rpc.Diagnostic.Event.Add (sanitize e)
          | Remove e -> Remove (sanitize e)
        in
        printfn "%s" (Dyn.to_string (Dune_rpc.Diagnostic.Event.to_dyn e))
      in
      Client.Handler.create
        ~diagnostic:(fun de ->
          List.iter de ~f:on_diagnostic_event;
          Fiber.return ())
        ()
    in
    run_client ~handler (fun client ->
        (* First we test for regular errors *)
        files [ ("dune-project", "(lang dune 3.0)") ];
        let* () =
          printfn "subscribing to notifications";
          Client.notification client Dune_rpc.Public.Notification.subscribe
            Diagnostics
        in
        f client)
  in
  run (fun () -> test exec)

let diagnostic_with_build setup target =
  setup_diagnostics (fun client ->
      files setup;
      dune_build client target)

let%expect_test "error in dune file" =
  diagnostic_with_build [ ("dune", "(library (name foo))") ] "foo.cma";
  [%expect
    {|
    subscribing to notifications
    Building foo.cma
    Build foo.cma succeeded
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
    subscribing to notifications
    Building foo.cma
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
    Build foo.cma failed
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
    subscribing to notifications
    Building (alias foo)
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
    Build (alias foo) failed
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
    subscribing to notifications
    Building (alias foo)
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
    Build (alias foo) failed
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
    subscribing to notifications
    Building ./foo.exe
    Build ./foo.exe succeeded
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
      subscribing to notifications
      Building ./foo
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
      Build ./foo failed
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
      let* () = dune_build client "./foo.exe" in
      [%expect
        {|
        subscribing to notifications
        Building ./foo.exe
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
        ]
        Build ./foo.exe failed |}];

      files [ ("foo.ml", "let () = print_endline \"foo\"") ];
      let+ () = dune_build client "./foo.exe" in
      [%expect
        {|
        Building ./foo.exe
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
        ]
        Build ./foo.exe succeeded |}]);
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
