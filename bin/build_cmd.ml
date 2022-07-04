open Stdune
open Import

let with_metrics ~common f =
  let start_time = Unix.gettimeofday () in
  Fiber.finalize f ~finally:(fun () ->
      let duration = Unix.gettimeofday () -. start_time in
      (if Common.print_metrics common then
       let gc_stat = Gc.quick_stat () in
       Console.print_user_message
         (User_message.make
            ([ Pp.textf "%s" (Memo.Perf_counters.report_for_current_run ())
             ; Pp.textf "(%.2fs total, %.1fM heap words)" duration
                 (float_of_int gc_stat.heap_words /. 1_000_000.)
             ; Pp.text "Timers:"
             ]
            @ List.map
                ~f:
                  (fun (timer, { Metrics.Timer.Measure.cumulative_time; count }) ->
                  Pp.textf "%s - time spent = %.2fs, count = %d" timer
                    cumulative_time count)
                (String.Map.to_list (Metrics.Timer.aggregated_timers ())))));
      Fiber.return ())

let run_build_system ~common ~request =
  let run ~(toplevel : unit Memo.Lazy.t) =
    with_metrics ~common (fun () ->
        Build_system.run (fun () -> Memo.Lazy.force toplevel))
  in
  let open Fiber.O in
  Fiber.finalize
    (fun () ->
      (* CR-someday amokhov: Currently we invalidate cached timestamps on every
         incremental rebuild. This conservative approach helps us to work around
         some [mtime] resolution problems (e.g. on Mac OS). It would be nice to
         find a way to avoid doing this. In fact, this may be unnecessary even
         for the initial build if we assume that the user does not modify files
         in the [_build] directory. For now, it's unclear if optimising this is
         worth the effort. *)
      Cached_digest.invalidate_cached_timestamps ();
      let* setup = Import.Main.setup () in
      let request =
        Action_builder.bind (Action_builder.of_memo setup) ~f:(fun setup ->
            request setup)
      in
      (* CR-someday cmoseley: Can we avoid creating a new lazy memo node every
         time the build system is rerun? *)
      (* This top-level node is used for traversing the whole Memo graph. *)
      let toplevel_cell, toplevel =
        Memo.Lazy.Expert.create ~name:"toplevel" (fun () ->
            let open Memo.O in
            let+ (), (_ : Dep.Fact.t Dep.Map.t) =
              Action_builder.run request Eager
            in
            ())
      in
      let* res = run ~toplevel in
      let+ () =
        match Common.dump_memo_graph_file common with
        | None -> Fiber.return ()
        | Some file ->
          let path = Path.of_filename_relative_to_initial_cwd file in
          let+ graph =
            Memo.dump_cached_graph
              ~time_nodes:(Common.dump_memo_graph_with_timing common)
              toplevel_cell
          in
          Graph.serialize graph ~path
            ~format:(Common.dump_memo_graph_format common)
        (* CR-someday cmoseley: It would be nice to use Persistent to dump a
           copy of the graph's internal representation here, so it could be used
           without needing to re-run the build*)
      in
      res)
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
  let rpc = Common.rpc common in
  Import.Scheduler.go_with_rpc_server_and_console_status_reporting ~common
    ~config (fun () ->
      Scheduler.Run.poll_passive
        ~get_build_request:
          (let+ (Build (targets, ivar)) =
             Dune_rpc_impl.Server.pending_build_action rpc
           in
           let request setup =
             Target.interpret_targets (Common.root common) config setup targets
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

let fmt =
  let doc = "Format source code." in
  let man =
    [ `S "DESCRIPTION"
    ; `P
        {|$(b,dune fmt) runs the formatter on the source code. The formatter is automatically selected. ocamlformat is used to format OCaml source code (*.ml and *.mli files) and refmt is used to format Reason source code (*.re and *.rei files).|}
    ; `Blocks Common.help_secs
    ]
  in
  let term =
    let+ common = Common.term in
    let common =
      Common.set_promote common Dune_engine.Clflags.Promote.Automatically
    in
    let config = Common.init common in
    let request (setup : Import.Main.build_system) =
      let dir = Path.(relative root) (Common.prefix_target common ".") in
      Alias.in_dir ~name:Dune_engine.Alias.Name.fmt ~recursive:true
        ~contexts:setup.contexts dir
      |> Alias.request
    in
    run_build_command ~common ~config ~request
  in
  (term, Term.info "fmt" ~doc ~man)
