open Stdune
open Fiber.O
open Dyn
open Common

let%expect_test "basics" =
  test unit (Fiber.return ());
  [%expect {| () |}];
  test
    unit
    (let* () = Fiber.return () in
     Fiber.return ());
  [%expect {| () |}];
  test
    unit
    (let* () = Scheduler.yield () in
     Fiber.return ());
  [%expect {| () |}]
;;

module Priority_queue_tests = struct
  module Queue = Fiber.Priority_queue

  let rec print_and_drain queue =
    match Queue.pop queue with
    | None -> ()
    | Some value ->
      print_endline value;
      print_and_drain queue
  ;;

  let%expect_test "priority ordering and FIFO tie-breaking" =
    let queue = Queue.create () in
    let first = Queue.create_priority queue in
    let second = Queue.create_priority queue in
    Queue.push queue first "first-1";
    Queue.push queue second "second-1";
    Queue.push queue first "first-2";
    Queue.push queue second "second-2";
    print_and_drain queue;
    [%expect
      {|
      first-1
      second-1
      first-2
      second-2 |}]
  ;;

  let%expect_test "LIFO ordering applies across and within shared handles" =
    let queue =
      Queue.create_with_order_key ~order_key:(fun { sequence; _ } -> sequence)
    in
    let shared = Queue.create_priority queue in
    let other = Queue.create_priority queue in
    Queue.push queue shared "first";
    Queue.push queue other "second";
    Queue.push queue shared "third";
    print_and_drain queue;
    [%expect
      {|
      third
      second
      first |}]
  ;;

  let%expect_test "random enqueue keys are deterministic" =
    let run () =
      let queue =
        Queue.create_with_order_key ~order_key:(fun { random_key; _ } -> random_key)
      in
      let priority = Queue.create_priority queue in
      List.iter [ "first"; "second"; "third"; "fourth" ] ~f:(fun value ->
        Queue.push queue priority value);
      let rec drain acc =
        match Queue.pop queue with
        | None -> List.rev acc
        | Some value -> drain (value :: acc)
      in
      drain []
    in
    let first = run () in
    let second = run () in
    printf
      "%b %b\n"
      (List.equal String.equal first second)
      (not (List.equal String.equal first [ "first"; "second"; "third"; "fourth" ]));
    [%expect {| true true |}]
  ;;

  let%expect_test "semantic rank precedes enqueue ordering" =
    let queue =
      Queue.create_with_order_key ~order_key:(fun { sequence; _ } -> sequence)
    in
    let low = Queue.create_priority queue in
    let high =
      Queue.create_rank
        ~rank:(Queue.Priority.make ~primary:0 ~secondary:1 ~tertiary:0)
        queue
    in
    Queue.push queue high "older-high";
    Queue.push queue low "newer-low";
    print_and_drain queue;
    [%expect
      {|
      older-high
      newer-low |}]
  ;;

  let%expect_test "increasing the priority of queued values" =
    let queue = Queue.create () in
    let first = Queue.create_priority queue in
    let second = Queue.create_priority queue in
    Queue.push queue first "first";
    Queue.push queue second "second";
    Queue.increase_priority second;
    print_and_drain queue;
    [%expect
      {|
      second
      first |}]
  ;;

  let%expect_test "setting queued priorities preserves FIFO age" =
    let queue = Queue.create () in
    let first = Queue.create_priority ~priority:2 queue in
    let second = Queue.create_priority ~priority:1 queue in
    Queue.push queue first "first";
    Queue.push queue second "second";
    Queue.set_priority first 0;
    print_and_drain queue;
    [%expect
      {|
      second
      first |}];
    let queue = Queue.create () in
    let first = Queue.create_priority queue in
    let second = Queue.create_priority queue in
    Queue.push queue first "first";
    Queue.push queue second "second";
    Queue.set_priority first 1;
    Queue.set_priority first 0;
    print_and_drain queue;
    [%expect
      {|
      first
      second |}]
  ;;

  let%expect_test "setting a shared or unqueued priority" =
    let queue = Queue.create () in
    let shared = Queue.create_priority ~priority:1 queue in
    let other = Queue.create_priority ~priority:1 queue in
    Queue.push queue shared "shared-1";
    Queue.push queue other "other";
    Queue.push queue shared "shared-2";
    Queue.set_priority shared 0;
    print_and_drain queue;
    [%expect
      {|
      other
      shared-1
      shared-2 |}];
    let queue = Queue.create () in
    let first = Queue.create_priority queue in
    let second = Queue.create_priority ~priority:1 queue in
    Queue.set_priority first 2;
    Queue.push queue second "second";
    Queue.push queue first "first";
    print_and_drain queue;
    [%expect
      {|
      first
      second |}]
  ;;

  let%expect_test "shared and initially elevated priorities" =
    let queue = Queue.create () in
    let shared = Queue.create_priority queue in
    let elevated = Queue.create_priority ~priority:2 queue in
    let normal = Queue.create_priority queue in
    Queue.push queue shared "shared-1";
    Queue.push queue normal "normal";
    Queue.push queue shared "shared-2";
    Queue.increase_priority shared;
    Queue.push queue elevated "elevated";
    print_and_drain queue;
    [%expect
      {|
      elevated
      shared-1
      shared-2
      normal |}]
  ;;

  let%expect_test "empty, length, and peek" =
    let queue = Queue.create () in
    let priority = Queue.create_priority queue in
    printf "%b %d\n" (Queue.is_empty queue) (Queue.length queue);
    Queue.increase_priority priority;
    Queue.push queue priority "value";
    printf
      "%b %d %s %s\n"
      (Queue.is_empty queue)
      (Queue.length queue)
      (Queue.peek queue |> Option.value_exn)
      (Queue.peek queue |> Option.value_exn);
    ignore (Queue.pop queue : string option);
    printf "%b %d\n" (Queue.is_empty queue) (Queue.length queue);
    let saturated = Queue.create_priority ~priority:Int.max_int queue in
    Queue.increase_priority_by saturated Int.max_int;
    printf "%b\n" (Queue.priority saturated = Int.max_int);
    let raised = Queue.create_priority ~priority:1 queue in
    Queue.increase_priority_by raised 3;
    printf "%d\n" (Queue.priority raised);
    [%expect
      {|
      true 0
      false 1 value value
      true 0
      true
      4 |}]
  ;;
end

module Throttle_tests = struct
  module Throttle = Fiber.Throttle

  let%expect_test "queued jobs use their current priorities" =
    let throttle = Throttle.create 1 in
    let first = Throttle.create_priority throttle in
    let second = Throttle.create_priority throttle in
    let blocker_started = Fiber.Ivar.create () in
    let release_blocker = Fiber.Ivar.create () in
    let run name priority =
      Throttle.run throttle ~priority (fun () ->
        print_endline name;
        Fiber.return ())
    in
    test
      unit
      (Fiber.fork_and_join_unit
         (fun () ->
            Throttle.run throttle (fun () ->
              let* () = Fiber.Ivar.fill blocker_started () in
              Fiber.Ivar.read release_blocker))
         (fun () ->
            let* () = Fiber.Ivar.read blocker_started in
            Fiber.parallel_iter
              [ (fun () -> run "first" first)
              ; (fun () -> run "second" second)
              ; (fun () ->
                  Throttle.increase_priority second;
                  Fiber.Ivar.fill release_blocker ())
              ]
              ~f:(fun f -> f ())));
    [%expect
      {|
      second
      first
      () |}]
  ;;

  let%expect_test "queued jobs observe priority demotion" =
    let throttle = Throttle.create 1 in
    let first = Throttle.create_priority ~priority:2 throttle in
    let second = Throttle.create_priority ~priority:1 throttle in
    let blocker_started = Fiber.Ivar.create () in
    let release_blocker = Fiber.Ivar.create () in
    let run name priority =
      Throttle.run throttle ~priority (fun () ->
        print_endline name;
        Fiber.return ())
    in
    test
      unit
      (Fiber.fork_and_join_unit
         (fun () ->
            Throttle.run throttle (fun () ->
              let* () = Fiber.Ivar.fill blocker_started () in
              Fiber.Ivar.read release_blocker))
         (fun () ->
            let* () = Fiber.Ivar.read blocker_started in
            Fiber.parallel_iter
              [ (fun () -> run "first" first)
              ; (fun () -> run "second" second)
              ; (fun () ->
                  Throttle.set_priority first 0;
                  Fiber.Ivar.fill release_blocker ())
              ]
              ~f:(fun f -> f ())));
    [%expect
      {|
      second
      first
      () |}]
  ;;

  let%expect_test "a high-priority chain keeps the released slot" =
    let throttle = Throttle.create 2 in
    let first = Throttle.create_priority ~priority:2 throttle in
    let second = Throttle.create_priority ~priority:2 throttle in
    let low = Throttle.create_priority throttle in
    let blocker_started = Fiber.Ivar.create () in
    let release_first = Fiber.Ivar.create () in
    let release_blocker = Fiber.Ivar.create () in
    let restarts = Queue.create () in
    let schedule_restart restart = Queue.push restarts restart in
    let rec process_restarts remaining =
      if remaining = 0
      then Fiber.return ()
      else
        let* () = Scheduler.yield () in
        match Queue.pop restarts with
        | None -> process_restarts remaining
        | Some restart ->
          (match Throttle.restart_waiters restart with
           | `Blocked -> Code_error.raise "unexpected blocked restart" []
           | `Ready waiters ->
             let* () =
               Fiber.sequential_iter waiters ~f:(fun ivar -> Fiber.Ivar.fill ivar ())
             in
             process_restarts (remaining - 1))
    in
    test
      unit
      (Fiber.parallel_iter
         [ (fun () ->
             let* () =
               Throttle.run throttle ~priority:first ~schedule_restart (fun () ->
                 print_endline "chain-1";
                 Fiber.Ivar.read release_first)
             in
             let* () =
               Throttle.run throttle ~priority:second ~schedule_restart (fun () ->
                 print_endline "chain-2";
                 Fiber.return ())
             in
             Fiber.Ivar.fill release_blocker ())
         ; (fun () ->
             Throttle.run throttle (fun () ->
               let* () = Fiber.Ivar.fill blocker_started () in
               Fiber.Ivar.read release_blocker))
         ; (fun () ->
             let* () = Fiber.Ivar.read blocker_started in
             Fiber.fork_and_join_unit
               (fun () ->
                  Throttle.run throttle ~priority:low (fun () ->
                    print_endline "low";
                    Fiber.return ()))
               (fun () -> Fiber.Ivar.fill release_first ()))
         ; (fun () -> process_restarts 2)
         ]
         ~f:(fun f -> f ()));
    [%expect
      {|
      chain-1
      chain-2
      low
      () |}]
  ;;

  let%expect_test "equal priority does not reserve a released slot" =
    let throttle = Throttle.create 1 in
    let priority = Throttle.create_priority ~priority:1 throttle in
    let running_started = Fiber.Ivar.create () in
    let release_running = Fiber.Ivar.create () in
    let restart_scheduled = ref false in
    test
      unit
      (Fiber.fork_and_join_unit
         (fun () ->
            Throttle.run
              throttle
              ~priority
              ~schedule_restart:(fun _ -> restart_scheduled := true)
              (fun () ->
                 let* () = Fiber.Ivar.fill running_started () in
                 Fiber.Ivar.read release_running))
         (fun () ->
            let* () = Fiber.Ivar.read running_started in
            Fiber.fork_and_join_unit
              (fun () ->
                 Throttle.run throttle ~priority (fun () ->
                   print_endline "waiter";
                   Fiber.return ()))
              (fun () ->
                 let* () = Scheduler.yield () in
                 Fiber.Ivar.fill release_running ())));
    printf "restart scheduled: %b\n" !restart_scheduled;
    [%expect
      {|
      waiter
      ()
      restart scheduled: false |}]
  ;;

  let%expect_test "demotion does not strand a deferred restart" =
    let throttle = Throttle.create 1 in
    let high = Throttle.create_priority ~priority:2 throttle in
    let low = Throttle.create_priority throttle in
    let high_started = Fiber.Ivar.create () in
    let release_high = Fiber.Ivar.create () in
    let restarts = Queue.create () in
    let rec process_restart () =
      let* () = Scheduler.yield () in
      match Queue.pop restarts with
      | None -> process_restart ()
      | Some restart ->
        Throttle.set_priority high 0;
        (match Throttle.restart_waiters restart with
         | `Blocked -> Code_error.raise "unexpected blocked restart" []
         | `Ready waiters ->
           Fiber.sequential_iter waiters ~f:(fun ivar -> Fiber.Ivar.fill ivar ()))
    in
    test
      unit
      (let* () =
         Fiber.parallel_iter
           [ (fun () ->
               Throttle.run
                 throttle
                 ~priority:high
                 ~schedule_restart:(fun restart -> Queue.push restarts restart)
                 (fun () ->
                    print_endline "high";
                    let* () = Fiber.Ivar.fill high_started () in
                    Fiber.Ivar.read release_high))
           ; (fun () ->
               let* () = Fiber.Ivar.read high_started in
               Fiber.fork_and_join_unit
                 (fun () ->
                    Throttle.run throttle ~priority:low (fun () ->
                      print_endline "low";
                      Fiber.return ()))
                 (fun () ->
                    let* () = Scheduler.yield () in
                    Fiber.Ivar.fill release_high ()))
           ; process_restart
           ]
           ~f:(fun f -> f ())
       in
       Throttle.run throttle (fun () ->
         print_endline "probe";
         Fiber.return ()));
    [%expect
      {|
      high
      low
      probe
      () |}]
  ;;

  let%expect_test "a restart blocker preserves the reserved slot" =
    let throttle = Throttle.create 1 in
    let high = Throttle.create_priority ~priority:1 throttle in
    let low = Throttle.create_priority throttle in
    let first_restart_blocker = Throttle.create_restart_blocker high in
    let second_restart_blocker = Throttle.create_restart_blocker high in
    let high_started = Fiber.Ivar.create () in
    let release_high = Fiber.Ivar.create () in
    let low_finished = Fiber.Ivar.create () in
    let restarts = Queue.create () in
    let rec process_restart () =
      let* () = Scheduler.yield () in
      match Queue.pop restarts with
      | None -> process_restart ()
      | Some restart ->
        (match Throttle.restart_waiters restart with
         | `Ready _ -> Code_error.raise "restart was not blocked" []
         | `Blocked ->
           print_endline "restart blocked";
           printf "running: %d\n" (Throttle.running throttle);
           Throttle.set_priority high 0;
           let* () = Throttle.resize throttle 2 in
           let* () = Fiber.Ivar.read low_finished in
           let* () = Throttle.resize throttle 1 in
           let restarts = Throttle.release_restart_blocker first_restart_blocker in
           if not (List.is_empty restarts)
           then Code_error.raise "restart released with a remaining blocker" [];
           print_endline "one blocker remains";
           let restarts = Throttle.release_restart_blocker second_restart_blocker in
           Fiber.sequential_iter restarts ~f:(fun restart ->
             match Throttle.restart_waiters restart with
             | `Blocked -> Code_error.raise "restart remained blocked" []
             | `Ready waiters ->
               Fiber.sequential_iter waiters ~f:(fun ivar -> Fiber.Ivar.fill ivar ())))
    in
    test
      unit
      (let* () =
         Fiber.parallel_iter
           [ (fun () ->
               Throttle.run
                 throttle
                 ~priority:high
                 ~schedule_restart:(fun restart -> Queue.push restarts restart)
                 (fun () ->
                    print_endline "high";
                    let* () = Fiber.Ivar.fill high_started () in
                    Fiber.Ivar.read release_high))
           ; (fun () ->
               let* () = Fiber.Ivar.read high_started in
               Fiber.fork_and_join_unit
                 (fun () ->
                    Throttle.run throttle ~priority:low (fun () ->
                      print_endline "low";
                      Fiber.Ivar.fill low_finished ()))
                 (fun () ->
                    let* () = Scheduler.yield () in
                    Fiber.Ivar.fill release_high ()))
           ; process_restart
           ]
           ~f:(fun f -> f ())
       in
       Throttle.run throttle (fun () ->
         print_endline "probe";
         Fiber.return ()));
    [%expect
      {|
      high
      restart blocked
      running: 0
      low
      one blocker remains
      probe
      () |}]
  ;;

  let%expect_test "a failing restart callback releases its reservation" =
    let throttle = Throttle.create 1 in
    let high = Throttle.create_priority ~priority:1 throttle in
    let low = Throttle.create_priority throttle in
    let high_started = Fiber.Ivar.create () in
    let release_high = Fiber.Ivar.create () in
    test
      unit
      (Fiber.fork_and_join_unit
         (fun () ->
            let* (_ : (unit, Exn_with_backtrace.t list) result) =
              Fiber.collect_errors (fun () ->
                Throttle.run
                  throttle
                  ~priority:high
                  ~schedule_restart:(fun _ -> raise Exit)
                  (fun () ->
                     print_endline "high";
                     let* () = Fiber.Ivar.fill high_started () in
                     Fiber.Ivar.read release_high))
            in
            Fiber.return ())
         (fun () ->
            let* () = Fiber.Ivar.read high_started in
            Fiber.fork_and_join_unit
              (fun () ->
                 Throttle.run throttle ~priority:low (fun () ->
                   print_endline "low";
                   Fiber.return ()))
              (fun () ->
                 let* () = Scheduler.yield () in
                 Fiber.Ivar.fill release_high ())));
    [%expect
      {|
      high
      low
      () |}]
  ;;

  let%expect_test "a failed job releases its slot" =
    let throttle = Throttle.create 1 in
    test
      unit
      (let* (_ : (unit, Exn_with_backtrace.t list) result) =
         Fiber.collect_errors (fun () -> Throttle.run throttle (fun () -> raise Exit))
       in
       Throttle.run throttle (fun () ->
         print_endline "slot released";
         Fiber.return ()));
    [%expect
      {|
      slot released
      () |}]
  ;;

  let%expect_test "constructing a job does not acquire a slot" =
    let throttle = Throttle.create 1 in
    ignore
      (Throttle.run throttle (fun () ->
         print_endline "unused";
         Fiber.return ())
       : unit Fiber.t);
    printf "%d\n" (Throttle.running throttle);
    test
      unit
      (Throttle.run throttle (fun () ->
         print_endline "slot available";
         Fiber.return ()));
    [%expect
      {|
      0
      slot available
      () |}]
  ;;

  let%expect_test "a priority cannot be used with another throttle" =
    let owner = Throttle.create 1 in
    let other = Throttle.create 1 in
    let priority = Throttle.create_priority owner in
    test
      unit
      (let* result =
         Fiber.collect_errors (fun () ->
           Throttle.run other ~priority (fun () -> Fiber.return ()))
       in
       (match result with
        | Ok () -> printf "accepted"
        | Error _ -> printf "rejected");
       printf " %d\n" (Throttle.running other);
       Throttle.run other (fun () ->
         print_endline "slot available";
         Fiber.return ()));
    [%expect
      {|
      rejected 0
      slot available
      () |}]
  ;;

  let%expect_test "resizing admits the current highest-priority job" =
    let throttle = Throttle.create 0 in
    let first = Throttle.create_priority ~priority:2 throttle in
    let second = Throttle.create_priority ~priority:1 throttle in
    let run name priority =
      Throttle.run throttle ~priority (fun () ->
        print_endline name;
        Fiber.return ())
    in
    test
      unit
      (Fiber.parallel_iter
         [ (fun () -> run "first" first)
         ; (fun () -> run "second" second)
         ; (fun () ->
             Throttle.set_priority first 0;
             Throttle.resize throttle 1)
         ]
         ~f:(fun f -> f ()));
    [%expect
      {|
      second
      first
      () |}]
  ;;
end

let%expect_test "fibers are reusable and thunks run during execution" =
  let runs = ref 0 in
  let fiber =
    Fiber.of_thunk (fun () ->
      incr runs;
      Fiber.return !runs)
  in
  printfn "before: %d" !runs;
  test int fiber;
  test int fiber;
  [%expect
    {|
    before: 0
    1
    2 |}]
;;

let%expect_test "of_thunk_apply fibers are lazy and reusable" =
  let runs = ref 0 in
  let fiber =
    Fiber.of_thunk_apply
      (fun x ->
         incr runs;
         Fiber.return (x + !runs))
      10
  in
  printfn "before: %d" !runs;
  test int fiber;
  test int fiber;
  [%expect
    {|
    before: 0
    11
    12 |}]
;;

let%expect_test "bind_apply threads its argument" =
  let fiber =
    Fiber.bind_apply
      (Fiber.return 10)
      (fun result argument ->
         printfn "callback %d %d" result argument;
         Fiber.return (result + argument))
      5
  in
  test int fiber;
  test int fiber;
  [%expect
    {|
    callback 10 5
    15
    callback 10 5
    15 |}]
;;

let%expect_test "map chains preserve callback order and exceptions" =
  let chain ~length ~raise_at =
    List.init length ~f:(fun i -> i + 1)
    |> List.fold_left ~init:(Fiber.return 0) ~f:(fun fiber i ->
      Fiber.map fiber ~f:(fun x ->
        printfn "map %d: %d" i x;
        if raise_at = Some i then failwith (Int.to_string i);
        x + 1))
  in
  List.iter [ 1; 2; 3; 4 ] ~f:(fun length ->
    printfn "length %d" length;
    test int (chain ~length ~raise_at:None));
  List.iter [ 1; 2; 3; 4 ] ~f:(fun raise_at ->
    printfn "raise at %d" raise_at;
    let result =
      Scheduler.run
        (Fiber.collect_errors (fun () -> chain ~length:4 ~raise_at:(Some raise_at)))
    in
    match result with
    | Error [ { exn = Failure message; _ } ] -> printfn "caught %s" message
    | Ok _ | Error _ -> print_endline "unexpected result");
  [%expect
    {|
    length 1
    map 1: 0
    1
    length 2
    map 1: 0
    map 2: 1
    2
    length 3
    map 1: 0
    map 2: 1
    map 3: 2
    3
    length 4
    map 1: 0
    map 2: 1
    map 3: 2
    map 4: 3
    4
    raise at 1
    map 1: 0
    caught 1
    raise at 2
    map 1: 0
    map 2: 1
    caught 2
    raise at 3
    map 1: 0
    map 2: 1
    map 3: 2
    caught 3
    raise at 4
    map 1: 0
    map 2: 1
    map 3: 2
    map 4: 3
    caught 4 |}]
;;

let%expect_test "collect_errors" =
  test (backtrace_result unit) (Fiber.collect_errors (fun () -> raise Exit));
  [%expect {| Error [ { exn = "Stdlib.Exit"; backtrace = "" } ] |}]
;;

let[@inline never] raise_for_backtrace_test _ = raise Exit

let%expect_test "scheduler exceptions preserve user backtraces" =
  let previously_recording = Printexc.backtrace_status () in
  let result =
    Exn.protect
      ~f:(fun () ->
        Printexc.record_backtrace true;
        Scheduler.run
          (Fiber.collect_errors (fun () ->
             Fiber.map (Fiber.return ()) ~f:raise_for_backtrace_test)))
      ~finally:(fun () -> Printexc.record_backtrace previously_recording)
  in
  let backtrace =
    match result with
    | Error [ { exn = Exit; backtrace } ] -> backtrace
    | result ->
      Code_error.raise
        "Unexpected result in Fiber backtrace test"
        [ "result", backtrace_result unit result ]
  in
  let contains_test_frame =
    match Printexc.backtrace_slots backtrace with
    | None -> false
    | Some slots ->
      Array.exists slots ~f:(fun slot ->
        match Printexc.Slot.location slot with
        | None -> false
        | Some { filename; _ } -> Filename.basename filename = "fiber_tests.ml")
  in
  printfn "nonempty: %b" (Printexc.raw_backtrace_length backtrace > 0);
  printfn "contains user frame: %b" contains_test_frame;
  [%expect
    {|
    nonempty: true
    contains user frame: true |}]
;;

let%expect_test "reraise_all" =
  let exns =
    let exn = Exn_with_backtrace.capture Exit in
    [ exn; exn; exn ]
  in
  let fail () = Fiber.reraise_all exns in
  test (backtrace_result unit) (Fiber.collect_errors fail);
  [%expect
    {|
    Error
      [ { exn = "Stdlib.Exit"; backtrace = "" }
      ; { exn = "Stdlib.Exit"; backtrace = "" }
      ; { exn = "Stdlib.Exit"; backtrace = "" }
      ] |}];
  test
    (backtrace_result unit)
    (Fiber.collect_errors (fun () ->
       Fiber.finalize fail ~finally:(fun () ->
         print_endline "finally";
         Fiber.return ())));
  [%expect
    {|
    finally
    Error
      [ { exn = "Stdlib.Exit"; backtrace = "" }
      ; { exn = "Stdlib.Exit"; backtrace = "" }
      ; { exn = "Stdlib.Exit"; backtrace = "" }
      ] |}];
  test
    unit
    ~expect_never:true
    (let+ _ = Fiber.reraise_all [] in
     print_endline "finish");
  [%expect
    {|
    [PASS] Never raised as expected |}]
;;

let%expect_test "collect_errors catches one error" =
  test (backtrace_result unit) (Fiber.collect_errors failing_fiber);
  [%expect
    {|
Error [ { exn = "Stdlib.Exit"; backtrace = "" } ]
|}]
;;

let%expect_test "collect_errors doesn't terminate on [never]" =
  test ~expect_never:true opaque (Fiber.collect_errors never_fiber);
  [%expect
    {|
[PASS] Never raised as expected
|}]
;;

let%expect_test "failing_fiber doesn't terminate" =
  test
    (backtrace_result unit)
    (Fiber.collect_errors (fun () ->
       let* () = failing_fiber () in
       failing_fiber ()));
  [%expect
    {|
Error [ { exn = "Stdlib.Exit"; backtrace = "" } ]
|}]
;;

let%expect_test "collect_errors fail one concurrent child fibers raises" =
  test
    (backtrace_result (pair unit unit))
    (Fiber.collect_errors (fun () -> Fiber.fork_and_join failing_fiber long_running_fiber));
  [%expect
    {|
Error [ { exn = "Stdlib.Exit"; backtrace = "" } ]
|}]
;;

let%expect_test "collect_errors can run concurrently" =
  test
    (pair (backtrace_result unit) unit)
    (Fiber.fork_and_join
       (fun () -> Fiber.collect_errors failing_fiber)
       long_running_fiber);
  [%expect
    {|
(Error [ { exn = "Stdlib.Exit"; backtrace = "" } ], ())
|}]
;;

let map_reduce_errors_unit ~on_error t =
  Fiber.map_reduce_errors (module Monoid.Unit) ~on_error t
;;

let%expect_test "collect errors inside with_error_handler" =
  test
    (unit_result (backtrace_result unit))
    ~expect_never:false
    (map_reduce_errors_unit
       ~on_error:(fun _ ->
         print_endline "captured the error";
         Fiber.return ())
       (fun () ->
          let* res = Fiber.collect_errors (fun () -> raise (Failure "")) in
          match res with
          | Ok () -> assert false
          | Error l ->
            printfn "got %d errors out of collect_errors" (List.length l);
            let* () = Fiber.reraise_all l in
            assert false));
  [%expect
    {|
    got 1 errors out of collect_errors
    captured the error
    Error () |}]
;;

let%expect_test "collect_errors restores the execution context properly" =
  let var = Fiber.Var.create None in
  test
    unit
    (Fiber.Var.set var (Some "a") (fun () ->
       let* _res =
         Fiber.Var.set var (Some "b") (fun () ->
           Fiber.collect_errors (fun () ->
             Fiber.Var.set var (Some "c") (fun () -> raise Exit)))
       in
       let* v = Fiber.Var.get_exn var in
       print_endline v;
       Fiber.return ()));
  [%expect
    {|
    a
    () |}]
;;

let%expect_test "handlers bubble up errors to parent handlers" =
  test
    ~expect_never:false
    (unit_result unit)
    (Fiber.fork_and_join_unit long_running_fiber (fun () ->
       let log_error by (e : Exn_with_backtrace.t) =
         Printf.printf "%s: raised %s\n" by (Printexc.to_string e.exn)
       in
       map_reduce_errors_unit
         ~on_error:(fun err ->
           log_error "outer" err;
           Fiber.return ())
         (fun () ->
            Fiber.fork_and_join_unit failing_fiber (fun () ->
              Fiber.with_error_handler
                ~on_error:(fun exn ->
                  log_error "inner" exn;
                  raise Exit)
                failing_fiber))));
  [%expect
    {|
    outer: raised Stdlib.Exit
    inner: raised Stdlib.Exit
    outer: raised Stdlib.Exit
    Error () |}]
;;

let%expect_test "nested with_error_handler" =
  let fiber =
    Fiber.with_error_handler
      ~on_error:(fun exn ->
        print_endline "outer handler";
        Exn_with_backtrace.reraise exn)
      (fun () ->
         Fiber.with_error_handler
           ~on_error:(fun exn ->
             print_endline "inner handler";
             Exn_with_backtrace.reraise exn)
           (fun () -> raise Exit))
  in
  (try test unit fiber with
   | Exit -> print_endline "[PASS] got Exit");
  [%expect
    {|
     inner handler
     outer handler
     [PASS] got Exit |}]
;;

let must_set_flag f =
  let flag = ref false in
  let setter () = flag := true in
  let check_set () =
    print_endline (if !flag then "[PASS] flag set" else "[FAIL] flag not set")
  in
  try
    f setter;
    check_set ()
  with
  | e ->
    check_set ();
    raise e
;;

let%expect_test "finalize" =
  let fiber =
    Fiber.finalize
      ~finally:(fun () -> Fiber.return (print_endline "finally"))
      (fun () -> Fiber.return ())
  in
  test unit fiber;
  [%expect
    {|
    finally
    ()
  |}];
  let fiber =
    Fiber.finalize
      ~finally:(fun () -> Fiber.return (print_endline "finally"))
      (fun () -> raise Exit)
  in
  (try test unit fiber with
   | Exit -> print_endline "[PASS] got Exit");
  [%expect
    {|
    finally
    [PASS] got Exit |}]
;;

let%expect_test "finalize combines body and finalizer errors" =
  let run ~body_fails ~finalizer_fails =
    printfn "body=%b finalizer=%b" body_fails finalizer_fails;
    test
      (backtrace_result int)
      (Fiber.collect_errors (fun () ->
         Fiber.finalize
           (fun () ->
              let* () = Scheduler.yield () in
              if body_fails then failwith "body" else Fiber.return 42)
           ~finally:(fun () ->
             let* () = Scheduler.yield () in
             if finalizer_fails then failwith "finalizer" else Fiber.return ())))
  in
  run ~body_fails:false ~finalizer_fails:false;
  run ~body_fails:true ~finalizer_fails:false;
  run ~body_fails:false ~finalizer_fails:true;
  run ~body_fails:true ~finalizer_fails:true;
  [%expect
    {|
    body=false finalizer=false
    Ok 42
    body=true finalizer=false
    Error [ { exn = "Failure(\"body\")"; backtrace = "" } ]
    body=false finalizer=true
    Error [ { exn = "Failure(\"finalizer\")"; backtrace = "" } ]
    body=true finalizer=true
    Error
      [ { exn = "Failure(\"body\")"; backtrace = "" }
      ; { exn = "Failure(\"finalizer\")"; backtrace = "" }
      ] |}]
;;

let%expect_test "finalize does not run the finalizer while the body is stalled" =
  test
    ~expect_never:true
    unit
    (Fiber.finalize never_fiber ~finally:(fun () ->
       print_endline "unexpected finalizer";
       Fiber.return ()));
  [%expect {| [PASS] Never raised as expected |}]
;;

let%expect_test "nested finalize" =
  let fiber =
    Fiber.finalize
      ~finally:(fun () -> Fiber.return (print_endline "outer finally"))
      (fun () ->
         Fiber.finalize
           ~finally:(fun () -> Fiber.return (print_endline "inner finally"))
           (fun () -> raise Exit))
  in
  (try test unit fiber with
   | Exit -> print_endline "[PASS] got Exit");
  [%expect
    {|
    inner finally
    outer finally
    [PASS] got Exit |}]
;;

let%expect_test "context switch and raise inside finalize" =
  let fiber =
    let mvar = Fiber.Mvar.create () in
    Fiber.fork_and_join_unit
      (fun () ->
         let* () = Fiber.Mvar.read mvar in
         printf "Hello from first fiber!\n";
         Fiber.Mvar.write mvar ())
      (fun () ->
         Fiber.finalize
           ~finally:(fun () -> Fiber.return (print_endline "finally"))
           (fun () ->
              let* () = Fiber.Mvar.write mvar () in
              let* () = Fiber.Mvar.read mvar in
              printf "raising in second fiber\n";
              raise Exit))
  in
  (try test unit fiber with
   | Exit -> print_endline "[PASS] got Exit");
  [%expect
    {|
    Hello from first fiber!
    raising in second fiber
    finally
    [PASS] got Exit |}]
;;

let%expect_test "sequential_iter error handling" =
  let fiber =
    Fiber.finalize
      ~finally:(fun () -> Fiber.return (print_endline "finally"))
      (fun () ->
         map_reduce_errors_unit
           (fun () ->
              Fiber.sequential_iter [ 1; 2; 3 ] ~f:(fun x ->
                if x = 2 then raise Exit else Fiber.return (Printf.printf "count: %d\n" x)))
           ~on_error:(fun exn_with_bt ->
             printf "exn: %s\n%!" (Printexc.to_string exn_with_bt.exn);
             Fiber.return ()))
  in
  test (unit_result unit) fiber ~expect_never:false;
  [%expect
    {|
    count: 1
    exn: Stdlib.Exit
    finally
    Error () |}]
;;

let%expect_test "sequential_iter" =
  let fiber =
    Fiber.finalize
      ~finally:(fun () -> Fiber.return (print_endline "finally"))
      (fun () ->
         Fiber.sequential_iter [ 1; 2; 3 ] ~f:(fun x ->
           Fiber.return (Printf.printf "count: %d\n" x)))
  in
  test unit fiber;
  [%expect
    {|
    count: 1
    count: 2
    count: 3
    finally
    () |}]
;;

let%expect_test _ =
  must_set_flag (fun setter ->
    test ~expect_never:true unit
    @@ Fiber.fork_and_join_unit never_fiber (fun () ->
      let* res = Fiber.collect_errors failing_fiber in
      print_dyn (backtrace_result unit res);
      let* () = long_running_fiber () in
      Fiber.return (setter ())));
  [%expect
    {|
    Error [ { exn = "Stdlib.Exit"; backtrace = "" } ]
    [PASS] Never raised as expected
    [PASS] flag set |}]
;;

let%expect_test _ =
  let forking_fiber () =
    Fiber.parallel_map [ 1; 2; 3; 4; 5 ] ~f:(fun x ->
      let* () = Scheduler.yield () in
      if x mod 2 = 1 then Fiber.return () else Printf.ksprintf failwith "%d" x)
  in
  must_set_flag (fun setter ->
    test ~expect_never:true unit
    @@ Fiber.fork_and_join_unit never_fiber (fun () ->
      let* res = Fiber.collect_errors forking_fiber in
      print_dyn (backtrace_result (list unit) res);
      let* () = long_running_fiber () in
      Fiber.return (setter ())));
  [%expect
    {|
    Error
      [ { exn = "Failure(\"2\")"; backtrace = "" }
      ; { exn = "Failure(\"4\")"; backtrace = "" }
      ]
    [PASS] Never raised as expected
    [PASS] flag set |}]
;;

let sorted_failures v =
  Result.map_error
    v
    ~f:
      (List.sort ~compare:(fun (x : Exn_with_backtrace.t) (y : Exn_with_backtrace.t) ->
         match x.exn, y.exn with
         | Failure x, Failure y -> String.compare x y
         | _, _ -> assert false))
;;

let%expect_test "fork - exceptions always thrown" =
  test
    (fun x -> sorted_failures x |> backtrace_result unit)
    (Fiber.collect_errors (fun () ->
       Fiber.fork_and_join_unit (fun () -> failwith "left") (fun () -> failwith "right")));
  [%expect
    {|
    Error
      [ { exn = "Failure(\"left\")"; backtrace = "" }
      ; { exn = "Failure(\"right\")"; backtrace = "" }
      ] |}]
;;

let test iter =
  test
    (fun x -> sorted_failures x |> backtrace_result unit)
    (Fiber.collect_errors (fun () ->
       iter [ 1; 2; 3 ] ~f:(fun x -> failwith (Int.to_string x))))
;;

let%expect_test "parallel_iter - all exceptions raised" =
  test Fiber.parallel_iter;
  [%expect
    {|
    Error
      [ { exn = "Failure(\"1\")"; backtrace = "" }
      ; { exn = "Failure(\"2\")"; backtrace = "" }
      ; { exn = "Failure(\"3\")"; backtrace = "" }
      ] |}]
;;

let%expect_test "sequential_iter - stop after first exception" =
  test Fiber.sequential_iter;
  [%expect
    {|
    Error [ { exn = "Failure(\"1\")"; backtrace = "" } ] |}]
;;

let%expect_test "parallel_map preserves input order" =
  let iv1 = Fiber.Ivar.create () in
  let iv2 = Fiber.Ivar.create () in
  let iv3 = Fiber.Ivar.create () in
  let map () =
    let+ result =
      Fiber.parallel_map
        [ 1, iv1; 2, iv2; 3, iv3 ]
        ~f:(fun (i, ivar) ->
          let+ () = Fiber.Ivar.read ivar in
          i)
    in
    printfn "%s" (String.concat ~sep:"," (List.map result ~f:Int.to_string))
  in
  let fill () =
    let* () = Scheduler.yield () in
    let* () = Fiber.Ivar.fill iv3 () in
    let* () = Fiber.Ivar.fill iv1 () in
    Fiber.Ivar.fill iv2 ()
  in
  Scheduler.run (Fiber.fork_and_join_unit map fill);
  [%expect {| 1,2,3 |}]
;;

let%expect_test "all_concurrently_unit" =
  Scheduler.run
    (let+ () = Fiber.all_concurrently_unit [] in
     printf "empty list");
  [%expect {| empty list |}];
  Scheduler.run
    (let+ () = Fiber.all_concurrently_unit [ Fiber.return () ] in
     printf "singleton list");
  [%expect {| singleton list |}];
  Scheduler.run
    (let print i =
       Fiber.of_thunk (fun () ->
         printfn "print: %i" i;
         Fiber.return ())
     in
     let+ () = Fiber.all_concurrently_unit [ print 1; print 2 ] in
     printf "multi element list");
  [%expect
    {|
    print: 1
    print: 2
    multi element list |}];
  Scheduler.run
    (let print i =
       Fiber.of_thunk (fun () ->
         printfn "print: %i" i;
         Fiber.return ())
     in
     let fail = Fiber.of_thunk (fun () -> raise Exit) in
     let+ () =
       let+ res =
         Fiber.collect_errors (fun () -> Fiber.all_concurrently_unit [ print 1; fail ])
       in
       match res with
       | Error [ { exn = Exit; _ } ] -> printfn "successfully caught error"
       | Ok () -> assert false
       | Error _ -> assert false
     in
     printf "multi element list");
  [%expect
    {|
    print: 1
    successfully caught error
    multi element list |}]
;;

let%expect_test "Lazy.force_all_unit handles empty and completed inputs" =
  Common.test unit (Fiber.Lazy.force_all_unit []);
  Common.test unit (Fiber.Lazy.force_all_unit [ Fiber.Lazy.unit; Fiber.Lazy.of_value () ]);
  [%expect
    {|
    ()
    () |}]
;;

let%expect_test "Lazy.force_all_unit starts every pending computation" =
  let gate = Fiber.Ivar.create () in
  let started = ref 0 in
  let make () =
    Fiber.Lazy.create (fun () ->
      incr started;
      Fiber.Ivar.read gate)
  in
  let a = make () in
  let b = make () in
  let force () =
    let+ () = Fiber.Lazy.force_all_unit [ a; b ] in
    print_endline "all forced"
  in
  let release () =
    let* () = Scheduler.yield () in
    printfn "started before release: %d" !started;
    Fiber.Ivar.fill gate ()
  in
  Scheduler.run (Fiber.fork_and_join_unit force release);
  printfn "values: %b %b" (Fiber.Lazy.is_value a) (Fiber.Lazy.is_value b);
  [%expect
    {|
    started before release: 2
    all forced
    values: true true |}]
;;

let%expect_test "Lazy.force_all_unit joins a computation already being forced" =
  let runs = ref 0 in
  let lazy_fiber =
    Fiber.Lazy.create (fun () ->
      incr runs;
      Scheduler.yield ())
  in
  Scheduler.run
    (Fiber.fork_and_join_unit
       (fun () -> Fiber.Lazy.force lazy_fiber)
       (fun () -> Fiber.Lazy.force_all_unit [ lazy_fiber ]));
  printfn "runs: %d" !runs;
  printfn "is value: %b" (Fiber.Lazy.is_value lazy_fiber);
  [%expect
    {|
    runs: 1
    is value: true |}]
;;

let%expect_test "Lazy.force_all_unit collects all failures" =
  let make name =
    Fiber.Lazy.create (fun () ->
      let* () = Scheduler.yield () in
      failwith name)
  in
  let a = make "a" in
  let b = make "b" in
  let run () =
    match
      Scheduler.run (Fiber.collect_errors (fun () -> Fiber.Lazy.force_all_unit [ a; b ]))
    with
    | Ok () -> print_endline "unexpected success"
    | Error errors ->
      let errors =
        List.map errors ~f:(fun { Exn_with_backtrace.exn; _ } ->
          match exn with
          | Failure message -> message
          | exn -> Printexc.to_string exn)
        |> List.sort ~compare:String.compare
      in
      printfn "errors: %s" (String.concat ~sep:"," errors)
  in
  run ();
  run ();
  printfn "values: %b %b" (Fiber.Lazy.is_value a) (Fiber.Lazy.is_value b);
  [%expect
    {|
    errors: a,b
    errors: a,b
    values: false false |}]
;;

let%expect_test "Mutex serializes fibers and releases after errors" =
  let mutex = Fiber.Mutex.create () in
  let inside = ref false in
  let max_inside = ref 0 in
  let inside_count = ref 0 in
  let critical_section () =
    Fiber.Mutex.with_lock mutex ~f:(fun () ->
      if !inside then print_endline "overlap";
      inside := true;
      incr inside_count;
      max_inside := max !max_inside !inside_count;
      let* () = Scheduler.yield () in
      decr inside_count;
      inside := false;
      Fiber.return ())
  in
  Scheduler.run (Fiber.parallel_iter [ (); (); () ] ~f:critical_section);
  printfn "max inside: %d" !max_inside;
  let result =
    Scheduler.run
      (Fiber.collect_errors (fun () ->
         Fiber.Mutex.with_lock mutex ~f:(fun () -> raise Exit)))
  in
  (match result with
   | Error [ { exn = Exit; _ } ] -> print_endline "caught Exit"
   | Ok () | Error _ -> print_endline "unexpected result");
  Scheduler.run
    (Fiber.Mutex.with_lock mutex ~f:(fun () ->
       print_endline "reacquired";
       Fiber.return ()));
  [%expect
    {|
    max inside: 1
    caught Exit
    reacquired |}]
;;
