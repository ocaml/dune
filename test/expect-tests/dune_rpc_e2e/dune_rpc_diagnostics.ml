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
  [%expect {|
    Building .
    Build . succeeded
    shutting down |}]

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
  if List.is_empty diagnostics then print_endline "<no diagnostics>"
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
  [%expect
    {|
    Building foo.cma
    Build foo.cma succeeded
    <no diagnostics> |}]

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
             does not match the interface .foo.objs/byte/foo.cmi: \n\
             Values do not match: val x : bool is not included in val x : int\n\
             The type bool is not compatible with the type int\n\
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
                                          " ] ]
            ]
          ]
        ]
      ; [ "targets"; [] ]
      ]
    ] |}]

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
              ; [ "pos_fname"; "$CWD/x" ]
              ; [ "pos_lnum"; "1" ]
              ]
            ]
          ; [ "stop"
            ; [ [ "pos_bol"; "0" ]
              ; [ "pos_cnum"; "0" ]
              ; [ "pos_fname"; "$CWD/x" ]
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
    ] |}]

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
              ; [ "pos_fname"; "$CWD/output.expected" ]
              ; [ "pos_lnum"; "1" ]
              ]
            ]
          ; [ "stop"
            ; [ [ "pos_bol"; "0" ]
              ; [ "pos_cnum"; "0" ]
              ; [ "pos_fname"; "$CWD/output.expected" ]
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
    ] |}]

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
    <no diagnostics> |}]

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
              ; [ "pos_fname"; "$CWD/dune" ]
              ; [ "pos_lnum"; "1" ]
              ]
            ]
          ; [ "stop"
            ; [ [ "pos_bol"; "0" ]
              ; [ "pos_cnum"; "49" ]
              ; [ "pos_fname"; "$CWD/dune" ]
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
    ] |}]

let%expect_test "library error location" =
  diagnostic_with_build
    [ ("dune", "(library (name foo) (libraries fake-library))")
    ; ("foo.ml", "")
    ]
    "./foo.cma";
  [%expect
    {|
    Building ./foo.cma
    Build ./foo.cma failed
    [ "Add"
    ; [ [ "id"; "0" ]
      ; [ "loc"
        ; [ [ "start"
            ; [ [ "pos_bol"; "0" ]
              ; [ "pos_cnum"; "31" ]
              ; [ "pos_fname"; "$CWD/dune" ]
              ; [ "pos_lnum"; "1" ]
              ]
            ]
          ; [ "stop"
            ; [ [ "pos_bol"; "0" ]
              ; [ "pos_cnum"; "43" ]
              ; [ "pos_fname"; "$CWD/dune" ]
              ; [ "pos_lnum"; "1" ]
              ]
            ]
          ]
        ]
      ; [ "message"
        ; [ "Verbatim"; "Error: Library \"fake-library\" not\n\
                         found.\n\
                         " ]
        ]
      ; [ "promotion"; [] ]
      ; [ "related"; [] ]
      ; [ "targets"; [] ]
      ]
    ] |}]

let%expect_test "create and fix error" =
  setup_diagnostics (fun client ->
      files
        [ ("dune", "(executable (name foo))")
        ; ("foo.ml", "let () = print_endline 123")
        ];
      let* poll = poll_exn client Dune_rpc.Public.Sub.diagnostic in
      let* () = print_diagnostics poll in
      [%expect {|
        <no diagnostics> |}];
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
                \  string\n\
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
                \  string\n\
                 "
              ]
            ]
          ; [ "promotion"; [] ]
          ; [ "related"; [] ]
          ; [ "targets"; [] ]
          ]
        ] |}]);
  [%expect {| |}]

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
          Build (alias foo) failed |}];
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
          promoted file contents:
          toto |}])
  in
  run (fun () -> with_dune_watch exec);
  [%expect {| |}]
