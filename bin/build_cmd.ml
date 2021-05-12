open Stdune
open Import

let run_build_system ~common ~(targets : unit -> Target.t list Memo.Build.t) =
  let build_started = Unix.gettimeofday () in
  Fiber.finalize
    (fun () ->
      Build_system.run (fun () ->
          let open Memo.Build.O in
          let* targets = targets () in
          Build_system.build (Target.request targets)))
    ~finally:(fun () ->
      (if Common.print_metrics common then
        let gc_stat = Gc.quick_stat () in
        Console.print_user_message
          (User_message.make
             [ Pp.textf "%s" (Memo.Perf_counters.report_for_current_run ())
             ; Pp.textf "(%.2f sec, %d heap words)"
                 (Unix.gettimeofday () -. build_started)
                 gc_stat.heap_words
             ]));
      Fiber.return ())

let run_build_command_poll ~(common : Common.t) ~config ~targets ~setup =
  let open Fiber.O in
  let every () =
    Cached_digest.invalidate_cached_timestamps ();
    let* setup = setup () in
    let* targets =
      match
        let open Option.O in
        let* rpc = Common.rpc common in
        Dune_rpc_impl.Server.pending_build_action rpc
      with
      | None -> Fiber.return (fun () -> targets setup)
      | Some (Build (targets, ivar)) ->
        let+ () = Fiber.Ivar.fill ivar Accepted in
        fun () ->
          Target.resolve_targets_exn (Common.root common) config setup targets
    in
    let+ () = run_build_system ~common ~targets in
    `Continue
  in
  Scheduler.poll ~common ~config ~every ~finally:Hooks.End_of_build.run

let run_build_command_once ~(common : Common.t) ~config ~targets ~setup =
  let open Fiber.O in
  let once () =
    let* setup = setup () in
    run_build_system ~common ~targets:(fun () -> targets setup)
  in
  Scheduler.go ~common ~config once

let run_build_command ~(common : Common.t) ~config ~targets =
  let setup () = Import.Main.setup () in
  (if Common.watch common then
    run_build_command_poll
  else
    run_build_command_once)
    ~setup ~common ~config ~targets

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
    let targets (setup : Import.Main.build_system) =
      Memo.Build.return
      @@ List.map dirs ~f:(fun dir ->
             let dir = Path.(relative root) (Common.prefix_target common dir) in
             Target.Alias
               (Alias.in_dir ~name:Dune_engine.Alias.Name.runtest
                  ~recursive:true ~contexts:setup.contexts dir))
    in
    run_build_command ~common ~config ~targets
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
    let targets setup =
      Target.resolve_targets_exn (Common.root common) config setup targets
    in
    run_build_command ~common ~config ~targets
  in
  (term, Term.info "build" ~doc ~man)
