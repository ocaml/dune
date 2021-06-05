open Stdune
open Import

let run_build_system ?report_error ~common ~(request : unit Action_builder.t) ()
    =
  let build_started = Unix.gettimeofday () in
  Fiber.finalize
    (fun () ->
      Build_system.run ?report_error (fun () -> Build_system.build request))
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

let run_build_command_poll ~(common : Common.t) ~config ~request ~setup =
  let open Fiber.O in
  let every ~report_error () =
    Cached_digest.invalidate_cached_timestamps ();
    let* setup = setup () in
    let* request =
      match
        let open Option.O in
        let* rpc = Common.rpc common in
        Dune_rpc_impl.Server.pending_build_action rpc
      with
      | None -> Fiber.return (request setup)
      | Some (Build (targets, ivar)) ->
        let+ () = Fiber.Ivar.fill ivar Accepted in
        Target.interpret_targets (Common.root common) config setup targets
    in
    let+ () = run_build_system ~report_error ~common ~request () in
    `Continue
  in
  Scheduler.poll ~common ~config ~every ~finally:Hooks.End_of_build.run

let run_build_command_once ~(common : Common.t) ~config ~request ~setup =
  let open Fiber.O in
  let once () =
    let* setup = setup () in
    run_build_system ~common ~request:(request setup) ()
  in
  Scheduler.go ~common ~config once

let run_build_command ~(common : Common.t) ~config ~request =
  let setup () = Import.Main.setup () in
  (if Common.watch common then
    run_build_command_poll
  else
    run_build_command_once)
    ~setup ~common ~config ~request

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
