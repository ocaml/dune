open Stdune
open Import

let run_build_system ~common ~(request : unit Action_builder.t) () =
  let build_started = Unix.gettimeofday () in
  Fiber.finalize
    (fun () ->
      Build_system.run (fun () ->
          let open Memo.Build.O in
          let+ (), _facts = Action_builder.run request Eager in
          ()))
    ~finally:(fun () ->
      (if Common.print_metrics common then
        let gc_stat = Gc.quick_stat () in
        Console.print_user_message
          (User_message.make
             [ Pp.textf "%s" (Memo.Perf_counters.report_for_current_run ())
             ; Pp.textf "(%.2fs total, %.2fs digests, %.1fM heap words)"
                 (Unix.gettimeofday () -. build_started)
                 (Metrics.Timer.read_seconds Digest.generic_timer)
                 (float_of_int gc_stat.heap_words /. 1_000_000.)
             ]));
      Fiber.return ())

let setup () = Import.Main.setup ()

let run_build_system ~common ~request =
  let open Fiber.O in
  Fiber.finalize
    (fun () ->
      Cached_digest.invalidate_cached_timestamps ();
      let* setup = setup () in
      let request =
        Action_builder.bind (Action_builder.memo_build setup) ~f:(fun setup ->
            request setup)
      in
      run_build_system ~common ~request ())
    ~finally:(fun () ->
      Hooks.End_of_build.run ();
      Fiber.return ())

let run_build_command_poll_eager ~(common : Common.t) ~config ~request : unit =
  Import.Scheduler.go_with_rpc_server_and_console_status_reporting ~common
    ~config (fun () -> Scheduler.Run.poll (run_build_system ~common ~request))

let run_build_command_poll_passive ~(common : Common.t) ~config ~request:_ :
    unit =
  (* CR-someday aalekseyev: It would've been better to complain if [request] is
     non-empty, but we can't check that here because [request] is a function.*)
  let open Fiber.O in
  match Common.rpc common with
  | None ->
    Code_error.raise
      "Attempted to start a passive polling mode without an RPC server" []
  | Some rpc ->
    Import.Scheduler.go_with_rpc_server_and_console_status_reporting ~common
      ~config (fun () ->
        Scheduler.Run.poll_passive
          ~get_build_request:
            (let+ (Build (targets, ivar)) =
               Dune_rpc_impl.Server.pending_build_action rpc
             in
             let request setup =
               Target.interpret_targets (Common.root common) config setup
                 targets
             in
             (run_build_system ~common ~request, ivar)))

let run_build_command_once ~(common : Common.t) ~config ~request =
  let open Fiber.O in
  let once () =
    let+ res = run_build_system ~common ~request in
    match res with
    | Error `Already_reported -> raise Dune_util.Report_error.Already_reported
    | Ok () -> ()
  in
  Scheduler.go ~common ~config once

let run_build_command ~(common : Common.t) ~config ~request =
  (match Common.watch common with
  | Yes Eager -> run_build_command_poll_eager
  | Yes Passive -> run_build_command_poll_passive
  | No -> run_build_command_once)
    ~common ~config ~request

let runtest =
  let doc = "Run tests." in
  let man =
    [ `S "DESCRIPTION"
    ; `P {|This is a short-hand for calling:|}
    ; `Pre {|  dune build @runtest|}
    ; `Blocks Common.help_secs
    ; Common.examples
        [ ( "Run all tests in the current source tree (including those that \
             passed on the last run)"
          , "dune runtest --force" )
        ; ( "Run tests sequentially without output buffering"
          , "dune runtest --no-buffer -j 1" )
        ]
    ]
  in
  let name_ = Arg.info [] ~docv:"DIR" in
  let term =
    let+ common = Common.term
    and+ dirs = Arg.(value & pos_all string [ "." ] name_) in
    let config = Common.init common in
    let request (setup : Import.Main.build_system) =
      Action_builder.all_unit
        (List.map dirs ~f:(fun dir ->
             let dir = Path.(relative root) (Common.prefix_target common dir) in
             Alias.in_dir ~name:Dune_engine.Alias.Name.runtest ~recursive:true
               ~contexts:setup.contexts dir
             |> Alias.request))
    in
    run_build_command ~common ~config ~request
  in
  (term, Term.info "runtest" ~doc ~man)

let build =
  let doc =
    "Build the given targets, or all installable targets if none are given."
  in
  let man =
    [ `S "DESCRIPTION"
    ; `P {|Targets starting with a $(b,@) are interpreted as aliases.|}
    ; `Blocks Common.help_secs
    ; Common.examples
        [ ("Build all targets in the current source tree", "dune build")
        ; ("Build targets in the `./foo/bar' directory", "dune build ./foo/bar")
        ; ( "Build the minimal set of targets required for tooling such as \
             Merlin (useful for quickly detecting errors)"
          , "dune build @check" )
        ; ( "Run all code formatting tools in-place"
          , "dune build --auto-promote @fmt" )
        ]
    ]
  in
  let name_ = Arg.info [] ~docv:"TARGET" in
  let term =
    let+ common = Common.term
    and+ targets = Arg.(value & pos_all dep [] name_) in
    let targets =
      match targets with
      | [] -> [ Common.default_target common ]
      | _ :: _ -> targets
    in
    let config = Common.init common in
    let request setup =
      Target.interpret_targets (Common.root common) config setup targets
    in
    run_build_command ~common ~config ~request
  in
  (term, Term.info "build" ~doc ~man)
