open Stdune
include Dune_scheduler
open Dune_tests_common
open Fiber.O

let () = init ()

let default =
  Clflags.display := Short;
  { Scheduler.Config.concurrency = 1
  ; priority_scheduling = false
  ; print_ctrl_c_warning = false
  ; watch_exclusions = []
  }
;;

let go ?(timeout = Time.Span.of_secs 0.3) ?(config = default) f =
  try Scheduler.Run.go ~timeout config ~file_watcher:No_watcher f with
  | Shutdown.E Requested -> ()
;;

let priority_config = { default with priority_scheduling = true }

let%expect_test "Action builder demand roots are eager and fresh" =
  let module Action_builder = Dune_engine.Action_builder in
  let module Demand_class = Memo.Job_priority.Demand_class in
  let observation =
    Memo.of_non_reproducible_fiber (Memo.Job_priority.For_tests.current_root ())
  in
  let builder =
    Action_builder.with_job_demand
      Demand_class.Direct
      (Action_builder.of_memo observation)
  in
  let result = ref None in
  go ~config:priority_config (fun () ->
    let* lazy_root, _ = Action_builder.evaluate_and_collect_deps builder |> Memo.run in
    let* eager_root_1, _ =
      Action_builder.evaluate_and_collect_facts builder |> Memo.run
    in
    let* eager_root_2, _ =
      Action_builder.evaluate_and_collect_facts builder |> Memo.run
    in
    result := Some (lazy_root, eager_root_1, eager_root_2);
    Fiber.return ());
  let demand_class = function
    | Demand_class.Bulk -> "bulk"
    | Demand_class.Normal -> "normal"
    | Demand_class.Direct -> "direct"
  in
  let root = function
    | None -> "none"
    | Some (demand_class_, _) -> demand_class demand_class_
  in
  let lazy_root, eager_root_1, eager_root_2 = Option.value_exn !result in
  Printf.printf "lazy: %s\n" (root lazy_root);
  Printf.printf "eager 1: %s\n" (root eager_root_1);
  Printf.printf "eager 2: %s\n" (root eager_root_2);
  let distinct_ids =
    match eager_root_1, eager_root_2 with
    | Some (_, id_1), Some (_, id_2) -> id_1 <> id_2
    | None, _ | _, None -> false
  in
  Printf.printf "distinct IDs: %b\n" distinct_ids;
  [%expect
    {|
    lazy: none
    eager 1: direct
    eager 2: direct
    distinct IDs: true |}]
;;

let run_with_demand demand_class memo =
  Memo.with_job_demand demand_class (fun () -> memo) |> Memo.run
;;

let%expect_test "the same root observes an active nested dependency once" =
  let module Demand_class = Memo.Job_priority.Demand_class in
  let inner_started = Fiber.Ivar.create () in
  let inspect_roots = Fiber.Ivar.create () in
  let inner =
    Memo.Lazy.create ~name:"same-root-inner" (fun () ->
      Memo.of_reproducible_fiber
        (let* () = Fiber.Ivar.fill inner_started () in
         let* () = Fiber.Ivar.read inspect_roots in
         let+ roots = Memo.Job_priority.For_tests.current_node_roots () in
         let root_id =
           match roots with
           | [ (Demand_class.Bulk, root_id) ] -> Some root_id
           | [] | [ ((Normal | Direct), _) ] | _ :: _ :: _ -> None
         in
         Printf.printf "one bulk root ID: %b\n" (Option.is_some root_id)))
  in
  let outer =
    Memo.Lazy.create ~name:"same-root-outer" (fun () -> Memo.Lazy.force inner)
  in
  let force_twice =
    Memo.with_job_demand Demand_class.Bulk (fun () ->
      Memo.parallel_map [ outer; outer ] ~f:Memo.Lazy.force)
  in
  go ~config:priority_config (fun () ->
    Fiber.fork_and_join_unit
      (fun () -> Memo.run force_twice >>| ignore)
      (fun () ->
         let* () = Fiber.Ivar.read inner_started in
         Fiber.Ivar.fill inspect_roots ()));
  [%expect {| one bulk root ID: true |}]
;;

let%expect_test "distinct same-class roots reach an active nested dependency" =
  let module Demand_class = Memo.Job_priority.Demand_class in
  let inner_started = Fiber.Ivar.create () in
  let inspect_roots = Fiber.Ivar.create () in
  let inner =
    Memo.Lazy.create ~name:"distinct-roots-inner" (fun () ->
      Memo.of_reproducible_fiber
        (let* () = Fiber.Ivar.fill inner_started () in
         let* () = Fiber.Ivar.read inspect_roots in
         let+ roots = Memo.Job_priority.For_tests.current_node_roots () in
         let classes =
           List.map roots ~f:(fun (demand_class, _) ->
             match demand_class with
             | Demand_class.Bulk -> "bulk"
             | Normal -> "normal"
             | Direct -> "direct")
           |> List.sort ~compare:String.compare
         in
         Printf.printf "roots: %s\n" (String.concat ~sep:"," classes)))
  in
  let outer =
    Memo.Lazy.create ~name:"distinct-roots-outer" (fun () -> Memo.Lazy.force inner)
  in
  go ~config:priority_config (fun () ->
    Fiber.fork_and_join_unit
      (fun () -> run_with_demand Demand_class.Bulk (Memo.Lazy.force outer))
      (fun () ->
         let* () = Fiber.Ivar.read inner_started in
         Fiber.fork_and_join_unit
           (fun () -> run_with_demand Demand_class.Bulk (Memo.Lazy.force outer))
           (fun () -> Fiber.Ivar.fill inspect_roots ())));
  [%expect {| roots: bulk,bulk |}]
;;

let%expect_test "late direct demand promotes nested work without leaking upward" =
  let module Demand_class = Memo.Job_priority.Demand_class in
  let order = ref [] in
  let record name = order := name :: !order in
  let inner_ready = Fiber.Ivar.create () in
  let inner =
    Memo.Lazy.create ~name:"shared-inner" (fun () ->
      Memo.of_reproducible_fiber
        (let* () = Fiber.Ivar.fill inner_ready () in
         Scheduler.with_job_slot (fun () ->
           let+ roots = Memo.Job_priority.For_tests.current_node_roots () in
           let classes =
             List.map roots ~f:(fun (demand_class, _) ->
               match demand_class with
               | Demand_class.Bulk -> "bulk"
               | Normal -> "normal"
               | Direct -> "direct")
             |> List.sort ~compare:String.compare
           in
           Printf.printf "shared roots: %s\n" (String.concat ~sep:"," classes);
           record "shared")))
  in
  let outer = Memo.Lazy.create ~name:"shared-outer" (fun () -> Memo.Lazy.force inner) in
  let bulk_caller =
    Memo.Lazy.create ~name:"bulk-caller" (fun () ->
      let open Memo.O in
      let* () = Memo.Lazy.force outer in
      Memo.of_reproducible_fiber
        (Scheduler.with_job_slot (fun () ->
           record "bulk-continuation";
           Fiber.return ())))
  in
  let normal =
    Memo.Lazy.create ~name:"normal" (fun () ->
      Memo.of_reproducible_fiber
        (Scheduler.with_job_slot (fun () ->
           record "normal";
           Fiber.return ())))
  in
  let blocker_started = Fiber.Ivar.create () in
  let release_blocker = Fiber.Ivar.create () in
  go ~config:priority_config (fun () ->
    Fiber.parallel_iter
      [ (fun () ->
          Scheduler.with_job_slot (fun () ->
            let* () = Fiber.Ivar.fill blocker_started () in
            Fiber.Ivar.read release_blocker))
      ; (fun () ->
          let* () = Fiber.Ivar.read blocker_started in
          run_with_demand Demand_class.Bulk (Memo.Lazy.force bulk_caller))
      ; (fun () ->
          let* () = Fiber.Ivar.read inner_ready in
          Fiber.fork_and_join_unit
            (fun () -> run_with_demand Demand_class.Normal (Memo.Lazy.force normal))
            (fun () ->
               Fiber.fork_and_join_unit
                 (fun () -> run_with_demand Demand_class.Direct (Memo.Lazy.force outer))
                 (fun () -> Fiber.Ivar.fill release_blocker ())))
      ]
      ~f:(fun f -> f ()));
  List.rev !order |> List.iter ~f:print_endline;
  [%expect
    {|
    shared roots: bulk,direct
    shared
    normal
    bulk-continuation |}]
;;

let%expect_test "demand classes propagate while restoring Memo dependencies" =
  let module Demand_class = Memo.Job_priority.Demand_class in
  let order = ref [] in
  let job name =
    let inner_node, inner =
      Memo.Lazy.Expert.create
        ~name:("restore-inner-" ^ name)
        ~cutoff:Unit.equal
        (fun () ->
           Memo.of_reproducible_fiber
             (Scheduler.with_job_slot (fun () ->
                order := name :: !order;
                Fiber.return ())))
    in
    let outer =
      Memo.Lazy.create ~name:("restore-outer-" ^ name) (fun () -> Memo.Lazy.force inner)
    in
    inner_node, outer
  in
  let bulk_node, bulk = job "bulk" in
  let direct_node, direct = job "direct" in
  go ~config:priority_config (fun () ->
    Memo.parallel_map [ bulk; direct ] ~f:Memo.Lazy.force |> Memo.run >>| ignore);
  order := [];
  Memo.reset
    (Memo.Invalidation.combine
       (Memo.Node.invalidate ~reason:Memo.Invalidation.Reason.Test bulk_node)
       (Memo.Node.invalidate ~reason:Memo.Invalidation.Reason.Test direct_node));
  let blocker_started = Fiber.Ivar.create () in
  let release_blocker = Fiber.Ivar.create () in
  go ~config:priority_config (fun () ->
    Fiber.fork_and_join_unit
      (fun () ->
         Scheduler.with_job_slot (fun () ->
           let* () = Fiber.Ivar.fill blocker_started () in
           Fiber.Ivar.read release_blocker))
      (fun () ->
         let* () = Fiber.Ivar.read blocker_started in
         Fiber.parallel_iter
           [ (fun () -> run_with_demand Demand_class.Bulk (Memo.Lazy.force bulk))
           ; (fun () -> run_with_demand Demand_class.Direct (Memo.Lazy.force direct))
           ; (fun () -> Fiber.Ivar.fill release_blocker ())
           ]
           ~f:(fun f -> f ())));
  List.rev !order |> List.iter ~f:print_endline;
  [%expect
    {|
    direct
    bulk |}]
;;

let%expect_test "a priority reservation survives asynchronous bookkeeping" =
  let order = ref [] in
  let record name = order := name :: !order in
  let low =
    Memo.Lazy.create ~name:"low" (fun () ->
      Memo.of_reproducible_fiber
        (Scheduler.with_job_slot (fun () ->
           record "low";
           Fiber.return ())))
  in
  let high =
    Memo.Lazy.create ~name:"high" (fun () ->
      Memo.of_reproducible_fiber
        (let* () =
           Scheduler.with_job_slot (fun () ->
             record "high-1";
             Fiber.return ())
         in
         let* () = Scheduler.async_exn (fun () -> Thread.delay 0.02) in
         let* result =
           Scheduler.async (fun () ->
             Thread.delay 0.02;
             raise Exit)
         in
         (match result with
          | Error _ -> ()
          | Ok () -> Code_error.raise "background failure was not reported" []);
         Scheduler.with_job_slot (fun () ->
           record "high-2";
           Fiber.return ())))
  in
  let module Demand_class = Memo.Job_priority.Demand_class in
  let blocker_started = Fiber.Ivar.create () in
  let release_blocker = Fiber.Ivar.create () in
  go ~config:priority_config (fun () ->
    Fiber.fork_and_join_unit
      (fun () ->
         Scheduler.with_job_slot (fun () ->
           let* () = Fiber.Ivar.fill blocker_started () in
           Fiber.Ivar.read release_blocker))
      (fun () ->
         let* () = Fiber.Ivar.read blocker_started in
         Fiber.parallel_iter
           [ (fun () -> run_with_demand Demand_class.Bulk (Memo.Lazy.force low))
           ; (fun () -> run_with_demand Demand_class.Direct (Memo.Lazy.force high))
           ; (fun () -> Fiber.Ivar.fill release_blocker ())
           ]
           ~f:(fun f -> f ())));
  List.rev !order |> List.iter ~f:print_endline;
  [%expect
    {|
    high-1
    high-2
    low |}]
;;

let%expect_test "priority scheduling is disabled by default" =
  let order = ref [] in
  let blocker_started = Fiber.Ivar.create () in
  let release_blocker = Fiber.Ivar.create () in
  go (fun () ->
    let* low = Scheduler.create_job_priority () in
    let* high = Scheduler.create_job_priority ~priority:2 () in
    let run name priority =
      Scheduler.with_job_slot ~priority (fun () ->
        order := name :: !order;
        Fiber.return ())
    in
    Fiber.fork_and_join_unit
      (fun () ->
         Scheduler.with_job_slot (fun () ->
           let* () = Fiber.Ivar.fill blocker_started () in
           Fiber.Ivar.read release_blocker))
      (fun () ->
         let* () = Fiber.Ivar.read blocker_started in
         Fiber.parallel_iter
           [ (fun () -> run "low" low)
           ; (fun () -> run "high" high)
           ; (fun () -> Fiber.Ivar.fill release_blocker ())
           ]
           ~f:(fun f -> f ())));
  List.rev !order |> List.iter ~f:print_endline;
  [%expect
    {|
    low
    high |}]
;;

let%expect_test "a deferred priority restart cannot strand waiters" =
  let blocker_started = Fiber.Ivar.create () in
  let release_high = Fiber.Ivar.create () in
  let release_blocker = Fiber.Ivar.create () in
  go
    ~timeout:(Time.Span.of_secs 1.0)
    ~config:{ priority_config with concurrency = 2 }
    (fun () ->
       let* high = Scheduler.create_job_priority ~priority:2 () in
       let* low = Scheduler.create_job_priority () in
       Fiber.parallel_iter
         [ (fun () ->
             Scheduler.with_job_slot ~priority:high (fun () ->
               Fiber.Ivar.read release_high))
         ; (fun () ->
             Scheduler.with_job_slot (fun () ->
               let* () = Fiber.Ivar.fill blocker_started () in
               Fiber.Ivar.read release_blocker))
         ; (fun () ->
             let* () = Fiber.Ivar.read blocker_started in
             Fiber.fork_and_join_unit
               (fun () ->
                  Scheduler.with_job_slot ~priority:low (fun () ->
                    print_endline "low resumed";
                    Fiber.Ivar.fill release_blocker ()))
               (fun () -> Fiber.Ivar.fill release_high ()))
         ]
         ~f:(fun f -> f ()));
  [%expect {| low resumed |}]
;;

let%expect_test "Memo priorities propagate through dependency chains" =
  let order = ref [] in
  let job name =
    Memo.Lazy.create ~name:("job-" ^ name) (fun () ->
      Memo.of_reproducible_fiber
        (Scheduler.with_job_slot (fun () ->
           order := name :: !order;
           Fiber.return ())))
  in
  let low = job "low" in
  let chain =
    List.init 3 ~f:Fun.id
    |> List.fold_left ~init:(job "chain-0") ~f:(fun dependency n ->
      let name = sprintf "chain-%d" (n + 1) in
      Memo.Lazy.create ~name:(name ^ "-consumer") (fun () ->
        let open Memo.O in
        let* () = Memo.Lazy.force dependency in
        Memo.of_reproducible_fiber
          (Scheduler.with_job_slot (fun () ->
             order := name :: !order;
             Fiber.return ()))))
  in
  let module Demand_class = Memo.Job_priority.Demand_class in
  let blocker_started = Fiber.Ivar.create () in
  let release_blocker = Fiber.Ivar.create () in
  go ~config:priority_config (fun () ->
    Fiber.fork_and_join_unit
      (fun () ->
         Scheduler.with_job_slot (fun () ->
           let* () = Fiber.Ivar.fill blocker_started () in
           Fiber.Ivar.read release_blocker))
      (fun () ->
         let* () = Fiber.Ivar.read blocker_started in
         Fiber.parallel_iter
           [ (fun () -> run_with_demand Demand_class.Bulk (Memo.Lazy.force low))
           ; (fun () -> run_with_demand Demand_class.Direct (Memo.Lazy.force chain))
           ; (fun () -> Fiber.Ivar.fill release_blocker ())
           ]
           ~f:(fun f -> f ())));
  List.rev !order |> List.iter ~f:print_endline;
  [%expect
    {|
    chain-0
    chain-1
    chain-2
    chain-3
    low |}]
;;

let%expect_test "raise inside Scheduler.Run.go" =
  (try
     (go
      @@ fun () ->
      Fiber.fork_and_join_unit
        (fun () ->
           print_endline "t1";
           Fiber.return ())
        (fun () -> raise Exit));
     assert false
   with
   | Dune_util.Report_error.Already_reported -> print_endline "--> exception observed");
  [%expect
    {|
    t1
    Error: exception Stdlib.Exit

    I must not crash.  Uncertainty is the mind-killer. Exceptions are the
    little-death that brings total obliteration.  I will fully express my cases.
    Execution will pass over me and through me.  And when it has gone past, I
    will unwind the stack along its path.  Where the cases are handled there will
    be nothing.  Only I will remain.
    --> exception observed |}]
;;

let canonical_signal_number signal =
  let signal = Signal.to_int signal in
  let previous_mask = Unix.sigprocmask SIG_UNBLOCK [ signal ] in
  Exn.protect
    ~finally:(fun () -> ignore (Unix.sigprocmask SIG_SETMASK previous_mask : int list))
    ~f:(fun () ->
      let mask_without_signal = Unix.sigprocmask SIG_BLOCK [] in
      ignore (Unix.sigprocmask SIG_BLOCK [ signal ] : int list);
      let mask_with_signal = Unix.sigprocmask SIG_BLOCK [] in
      match
        List.filter mask_with_signal ~f:(fun signal ->
          not (List.mem mask_without_signal signal ~equal:Int.equal))
      with
      | [ signal ] -> signal
      | mask ->
        Code_error.raise
          "could not determine the signal's mask representation"
          [ "mask", Dyn.list Dyn.int mask ])
;;

let terminal_signals_are_blocked terminal_signals =
  let mask = Unix.sigprocmask SIG_BLOCK [] in
  List.for_all terminal_signals ~f:(fun signal -> List.mem mask signal ~equal:Int.equal)
;;

let%expect_test "threaded console handles terminal signals in the console thread" =
  let terminal_signals = List.map Terminal_signals.signals ~f:Signal.to_int in
  let terminal_signals_in_mask =
    List.map Terminal_signals.signals ~f:canonical_signal_number
  in
  let console_thread_blocked = ref None in
  let observation_mutex = Mutex.create () in
  let observed = Condition.create () in
  let observe_from_console_thread () =
    Mutex.protect observation_mutex (fun () ->
      if Option.is_none !console_thread_blocked
      then (
        console_thread_blocked
        := Some (terminal_signals_are_blocked terminal_signals_in_mask);
        Condition.broadcast observed))
  in
  let wait_for_console_thread_observation () =
    Mutex.protect observation_mutex (fun () ->
      while Option.is_none !console_thread_blocked do
        Condition.wait observed observation_mutex
      done;
      Option.value_exn !console_thread_blocked)
  in
  let previous_mask = Unix.sigprocmask SIG_BLOCK [] in
  Exn.protect
    ~finally:(fun () ->
      Console.Backend.set Console.Backend.dumb;
      ignore (Unix.sigprocmask SIG_SETMASK previous_mask : int list))
    ~f:(fun () ->
      ignore (Unix.sigprocmask SIG_UNBLOCK terminal_signals : int list);
      Printf.printf
        "main before start blocked: %b\n"
        (terminal_signals_are_blocked terminal_signals_in_mask);
      let module Test_console = struct
        let start () = ()
        let render (_ : Dune_threaded_console.state) = ()

        let handle_user_events ~now ~time_budget:_ (_ : Mutex.t) _ =
          observe_from_console_thread ();
          Unix.sleepf 0.01;
          now
        ;;

        let reset () = ()
        let reset_flush_history () = ()
        let finish () = ()
      end
      in
      Console.Backend.set
        (Dune_threaded_console.make ~frames_per_second:60 (module Test_console));
      Printf.printf
        "main after start blocked: %b\n"
        (terminal_signals_are_blocked terminal_signals_in_mask);
      Printf.printf
        "console thread blocked: %b\n"
        (wait_for_console_thread_observation ()));
  [%expect
    {|
    main before start blocked: false
    main after start blocked: true
    console thread blocked: false
    |}]
;;
