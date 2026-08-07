open! Stdune
open Test_helpers.Make ()

let in_non_incremental_mode ~f =
  Memo.Metrics.reset ();
  Memo.set_incremental false;
  Exn.protect ~f ~finally:(fun () ->
    Memo.Metrics.reset ();
    Memo.set_incremental true)
;;

let%expect_test "non-incremental mode discards dependencies but counts them" =
  in_non_incremental_mode ~f:(fun () ->
    let dependency =
      Memo.lazy_node ~name:"dependency" (fun () ->
        Scheduler.yield () |> Memo.of_reproducible_fiber)
    in
    let node =
      Memo.lazy_node ~name:"node" (fun () ->
        Memo.parallel_iter [ dependency; dependency ] ~f:Memo.Node.read)
    in
    let () = run (Memo.Node.read node) in
    let deps =
      Memo.For_tests.get_deps_structured node |> Option.value_exn |> Dyn.to_string
    in
    printfn "dependencies: %s" deps;
    printfn "edges: %d" (Counter.read Memo.Metrics.Compute.edges);
    printfn "cycle detection edges: %d" (Counter.read Memo.Metrics.Cycle_detection.edges);
    Memo.Metrics.assert_invariants ());
  [%expect
    {|
    dependencies: Empty
    edges: 2
    cycle detection edges: 1
    |}]
;;

let%expect_test "non-incremental mode preserves early errors outside Memo nodes" =
  in_non_incremental_mode ~f:(fun () ->
    let trace = ref [] in
    let log event = trace := event :: !trace in
    let (_ : (unit, unit) result) =
      Scheduler.run
        (Fiber.map_reduce_errors
           (module Monoid.Unit)
           ~on_error:(fun _exn ->
             log "late";
             Fiber.return ())
           (fun () ->
              Memo.run_with_error_handler
                (fun () ->
                   Memo.fork_and_join_unit
                     (fun () ->
                        Fiber.map (Scheduler.yield ()) ~f:(fun () -> failwith "error")
                        |> Memo.of_reproducible_fiber)
                     (fun () ->
                        Fiber.map (Scheduler.yield ()) ~f:(fun () -> log "other branch")
                        |> Memo.of_reproducible_fiber))
                ~handle_error_no_raise:(fun _exn ->
                  log "early";
                  Fiber.return ())))
    in
    List.rev !trace |> List.iter ~f:(fun event -> printfn "%s" event));
  [%expect
    {|
    early
    other branch
    late
    |}]
;;
