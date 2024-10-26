open Import

let with_metrics ~common f =
  let start_time = Unix.gettimeofday () in
  Fiber.finalize f ~finally:(fun () ->
    let duration = Unix.gettimeofday () -. start_time in
    if Common.print_metrics common
    then (
      let gc_stat = Gc.quick_stat () in
      (* We reset Memo counters below, unconditionally. *)
      let memo_counters_report = Memo.Metrics.report ~reset_after_reporting:false in
      Console.print_user_message
        (User_message.make
           ([ Pp.textf "%s" memo_counters_report
            ; Pp.textf
                "(%.2fs total, %.1fM heap words)"
                duration
                (float_of_int gc_stat.heap_words /. 1_000_000.)
            ; Pp.text "Timers:"
            ]
            @ List.map
                ~f:(fun (timer, { Metrics.Timer.Measure.cumulative_time; count }) ->
                  Pp.textf
                    "%s - time spent = %.2fs, count = %d"
                    timer
                    cumulative_time
                    count)
                (String.Map.to_list (Metrics.Timer.aggregated_timers ())))));
    Memo.Metrics.reset ();
    Fiber.return ())
;;

let run_build_system ~common ~request =
  let run ~(toplevel : unit Memo.Lazy.t) =
    with_metrics ~common (fun () -> build (fun () -> Memo.Lazy.force toplevel))
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
        Action_builder.bind (Action_builder.of_memo setup) ~f:(fun setup -> request setup)
      in
      (* CR-someday cmoseley: Can we avoid creating a new lazy memo node every
         time the build system is rerun? *)
      (* This top-level node is used for traversing the whole Memo graph. *)
      let toplevel_cell, toplevel =
        Memo.Lazy.Expert.create ~name:"toplevel" (fun () ->
          let open Memo.O in
          let+ (), (_ : Dep.Fact.t Dep.Map.t) =
            Action_builder.evaluate_and_collect_facts request
          in
          ())
      in
      let* res = run ~toplevel in
      let+ () =
        match Common.dump_memo_graph_file common with
        | None -> Fiber.return ()
        | Some file ->
          let path = Path.external_ file in
          let+ graph =
            Memo.dump_cached_graph
              ~time_nodes:(Common.dump_memo_graph_with_timing common)
              toplevel_cell
          in
          Graph.serialize graph ~path ~format:(Common.dump_memo_graph_format common)
        (* CR-someday cmoseley: It would be nice to use Persistent to dump a
           copy of the graph's internal representation here, so it could be used
           without needing to re-run the build*)
      in
      res)
    ~finally:(fun () ->
      Hooks.End_of_build.run ();
      Fiber.return ())
;;

let run_build_command_poll_eager ~(common : Common.t) ~config ~request : unit =
  Scheduler.go_with_rpc_server_and_console_status_reporting ~common ~config (fun () ->
    Scheduler.Run.poll (run_build_system ~common ~request))
;;

let run_build_command_poll_passive ~(common : Common.t) ~config ~request:_ : unit =
  (* CR-someday aalekseyev: It would've been better to complain if [request] is
     non-empty, but we can't check that here because [request] is a function.*)
  let open Fiber.O in
  let rpc =
    match Common.rpc common with
    | `Allow server -> server
    | `Forbid_builds -> Code_error.raise "rpc server must be allowed in passive mode" []
  in
  Scheduler.go_with_rpc_server_and_console_status_reporting ~common ~config (fun () ->
    Scheduler.Run.poll_passive
      ~get_build_request:
        (let+ (Build (targets, ivar)) = Dune_rpc_impl.Server.pending_build_action rpc in
         let request setup =
           Target.interpret_targets (Common.root common) config setup targets
         in
         run_build_system ~common ~request, ivar))
;;

let run_build_command_once ~(common : Common.t) ~config ~request =
  let open Fiber.O in
  let once () =
    let+ res = run_build_system ~common ~request in
    match res with
    | Error `Already_reported -> raise Dune_util.Report_error.Already_reported
    | Ok () -> ()
  in
  Scheduler.go ~common ~config once
;;

let run_build_command ~(common : Common.t) ~config ~request =
  (match Common.watch common with
   | Yes Eager -> run_build_command_poll_eager
   | Yes Passive -> run_build_command_poll_passive
   | No -> run_build_command_once)
    ~common
    ~config
    ~request
;;

let build =
  let doc = "Build the given targets, or the default ones if none are given." in
  let man =
    [ `S "DESCRIPTION"
    ; `P {|Targets starting with a $(b,@) are interpreted as aliases.|}
    ; `Blocks Common.help_secs
    ; Common.examples
        [ "Build all targets in the current source tree", "dune build"
        ; "Build targets in the `./foo/bar' directory", "dune build ./foo/bar"
        ; ( "Build the minimal set of targets required for tooling such as Merlin \
             (useful for quickly detecting errors)"
          , "dune build @check" )
        ; "Run all code formatting tools in-place", "dune build --auto-promote @fmt"
        ]
    ]
  in
  let name_ = Arg.info [] ~docv:"TARGET" in
  let term =
    let+ builder = Common.Builder.term
    and+ targets = Arg.(value & pos_all dep [] name_) in
    let targets =
      match targets with
      | [] -> [ Common.Builder.default_target builder ]
      | _ :: _ -> targets
    in
    let common, config = Common.init builder in
    let request setup =
      Target.interpret_targets (Common.root common) config setup targets
    in
    run_build_command ~common ~config ~request
  in
  Cmd.v (Cmd.info "build" ~doc ~man ~envs:Common.envs) term
;;
