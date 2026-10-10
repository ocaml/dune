open Stdune
module Caml_lazy = Lazy

module Scheduler = struct
  let t = Test_scheduler.create ()
  let yield () = Test_scheduler.yield t
  let run f = Test_scheduler.run t f
end

let rec delay_n_time_units counter n =
  if n = 0
  then Fiber.return ()
  else (
    assert (n > 0);
    Fiber.bind (Scheduler.yield ()) ~f:(fun () ->
      incr counter;
      delay_n_time_units counter (n - 1)))
;;

let make_shared_error_nodes f =
  let error_node = Memo.Lazy.create ~name:"error-node" f in
  let n1 =
    Memo.Lazy.create ~name:"error-node-wrapper-1" (fun () -> Memo.Lazy.force error_node)
  in
  let n2 =
    Memo.Lazy.create ~name:"error-node-wrapper-2" (fun () -> Memo.Lazy.force error_node)
  in
  n1, n2
;;

(* This test demonstrates that [Memo.run_with_error_handler] does indeed return
   exceptions early, but it also demonstrates a problem where if you run
   multiple instances of [run_with_error_handler] in parallel then some of them
   get their errors delayed. *)
let%expect_test "Memo.run_with_error_handler" =
  let time_counter = ref 0 in
  let n1, n2 =
    make_shared_error_nodes (fun () ->
      Memo.of_reproducible_fiber
        (Fiber.fork_and_join_unit
           (fun () -> Fiber.map (Scheduler.yield ()) ~f:(fun () -> failwith "Error_node"))
           (fun () -> delay_n_time_units time_counter 10)))
  in
  let run_memo_and_collect_errors m =
    let trace = ref [] in
    let log s = trace := (s, !time_counter) :: !trace in
    Fiber.map
      (Fiber.map_reduce_errors
         (module Monoid.Unit)
         ~on_error:(fun _exn ->
           log "late";
           Fiber.return ())
         (fun () ->
            Memo.run_with_error_handler m ~handle_error_no_raise:(fun _exn ->
              log "early";
              Fiber.return ())))
      ~f:(fun _result -> !trace)
  in
  let trace1, trace2 =
    Scheduler.run
      (Fiber.fork_and_join
         (fun () -> run_memo_and_collect_errors (fun () -> Memo.Lazy.force n1))
         (fun () -> run_memo_and_collect_errors (fun () -> Memo.Lazy.force n2)))
  in
  let print_trace l =
    List.iter (List.rev l) ~f:(fun (what, when_) -> Printf.printf "%s@%d\n" what when_)
  in
  print_trace trace1;
  print_trace trace2;
  [%expect
    {|
early@0
late@10
early@10
late@10
  |}]
;;

let%expect_test "one error handler scope for several computations" =
  let n1, n2 =
    make_shared_error_nodes (fun () ->
      Memo.of_reproducible_fiber
        (Fiber.map (Scheduler.yield ()) ~f:(fun () -> failwith "Error_node")))
  in
  let reports = ref 0 in
  let result1, result2 =
    Scheduler.run
      (Memo.with_error_handler
         ~handle_error_no_raise:(fun _exn ->
           incr reports;
           Fiber.return ())
         (fun () ->
            Fiber.fork_and_join
              (fun () -> Memo.run_and_collect_errors (fun () -> Memo.Lazy.force n1))
              (fun () -> Memo.run_and_collect_errors (fun () -> Memo.Lazy.force n2))))
  in
  let error_count = function
    | Ok () -> 0
    | Error errors -> Memo.Exn_set.to_list errors |> List.length
  in
  let unique_errors =
    [ result1; result2 ]
    |> List.filter_map ~f:(function
      | Ok () -> None
      | Error errors -> Some errors)
    |> Memo.Exn_set.union_all
    |> Memo.Exn_set.to_list
    |> List.length
  in
  Printf.printf
    "reports: %d\nfirst errors: %d\nsecond errors: %d\nunique errors: %d\n"
    !reports
    (error_count result1)
    (error_count result2)
    unique_errors;
  [%expect
    {|
reports: 1
first errors: 1
second errors: 1
unique errors: 1
  |}]
;;
