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

(* This test demonstrates that [Memo.run_with_error_handler] does indeed return
   exceptions early, but it also demonstrates a problem where if you run
   multiple instances of [run_with_error_handler] in parallel then some of them
   get their errors delayed. *)
let%expect_test "Memo.run_with_error_handler" =
  let time_counter = ref 0 in
  let error_node =
    Memo.Lazy.create ~name:"error-node" (fun () ->
      Memo.of_reproducible_fiber
        (Fiber.fork_and_join_unit
           (fun () -> Fiber.map (Scheduler.yield ()) ~f:(fun () -> failwith "Error_node"))
           (fun () -> delay_n_time_units time_counter 10)))
  in
  let n1 =
    Memo.Lazy.create ~name:"error-node-wrapper-1" (fun () -> Memo.Lazy.force error_node)
  in
  let n2 =
    Memo.Lazy.create ~name:"error-node-wrapper-2" (fun () -> Memo.Lazy.force error_node)
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

let%expect_test "collected user errors stay silent, remain reusable and invalidate" =
  let broken = Memo.Var.create ~name:"broken-input" true in
  let source =
    Memo.lazy_ ~name:"prepared-input" ~cutoff:Int.equal (fun () ->
      let open Memo.O in
      let+ broken = Memo.Var.read broken in
      if broken then User_error.raise [ Pp.text "broken input" ];
      42)
  in
  let collected =
    Memo.lazy_ ~name:"collected-input" (fun () ->
      Memo.collect_user_errors (Memo.Lazy.force source))
  in
  let run computation =
    Scheduler.run
      (Fiber.collect_errors (fun () ->
         Memo.run_with_error_handler
           (fun () -> computation)
           ~handle_error_no_raise:(fun _ ->
             print_endline "reported";
             Fiber.return ())))
  in
  let show () =
    match run (Memo.Lazy.force collected) with
    | Ok (Ok value) -> Printf.printf "value: %d\n" value
    | Ok (Error messages) -> Printf.printf "collected: %d\n" (List.length messages)
    | Error errors -> Printf.printf "unexpected: %d\n" (List.length errors)
  in
  show ();
  show ();
  [%expect
    {|
    collected: 1
    collected: 1
  |}];
  (match run (Memo.Lazy.force source) with
   | Error errors -> Printf.printf "raised: %d\n" (List.length errors)
   | Ok _ -> print_endline "unexpected success");
  [%expect
    {|
    reported
    raised: 1
  |}];
  Memo.reset (Memo.Var.set broken false);
  show ();
  [%expect {| value: 42 |}];
  Memo.reset (Memo.Var.set broken true);
  show ();
  [%expect {| collected: 1 |}]
;;

let%expect_test "shared errors can be collected and demanded concurrently" =
  List.iter [ true; false ] ~f:(fun collect_first ->
    let reported = ref 0 in
    let captured = ref 0 in
    let source =
      Memo.lazy_ ~name:"shared-error" (fun () ->
        User_error.raise [ Pp.text "shared failure" ])
    in
    let result =
      Scheduler.run
        (Fiber.collect_errors (fun () ->
           Memo.run_with_error_handler
             (fun () ->
                Memo.parallel_iter [ collect_first; not collect_first ] ~f:(fun collect ->
                  if collect
                  then
                    let open Memo.O in
                    let+ result = Memo.collect_user_errors (Memo.Lazy.force source) in
                    match result with
                    | Ok () -> ()
                    | Error messages -> captured := List.length messages
                  else Memo.Lazy.force source))
             ~handle_error_no_raise:(fun _ ->
               incr reported;
               Fiber.return ())))
    in
    let raised =
      match result with
      | Ok () -> 0
      | Error errors -> List.length errors
    in
    Printf.printf "captured: %d, reported: %d, raised: %d\n" !captured !reported raised);
  [%expect
    {|
    captured: 1, reported: 1, raised: 1
    captured: 1, reported: 1, raised: 1
  |}]
;;

let%expect_test "collecting user errors does not swallow internal errors" =
  let source = Memo.lazy_ ~name:"internal-error" (fun () -> Code_error.raise "bug" []) in
  let result =
    Scheduler.run
      (Fiber.collect_errors (fun () ->
         Memo.run_with_error_handler
           (fun () -> Memo.collect_user_errors (Memo.Lazy.force source))
           ~handle_error_no_raise:(fun _ ->
             print_endline "reported";
             Fiber.return ())))
  in
  (match result with
   | Error errors -> Printf.printf "raised: %d\n" (List.length errors)
   | Ok _ -> print_endline "unexpected success");
  [%expect
    {|
    reported
    raised: 1
  |}]
;;

let%expect_test "non-user cancellation is not collected" =
  let source =
    Memo.lazy_ ~name:"cancelled" (fun () -> raise (Memo.Non_reproducible Exit))
  in
  let result =
    Scheduler.run
      (Fiber.collect_errors (fun () ->
         Memo.run (Memo.collect_user_errors (Memo.Lazy.force source))))
  in
  (match result with
   | Error [ { Exn_with_backtrace.exn = Memo.Error.E error; _ } ] ->
     (match Memo.Error.get error with
      | Exit -> print_endline "cancelled"
      | _ -> print_endline "unexpected error")
   | Error _ -> print_endline "unexpected errors"
   | Ok _ -> print_endline "unexpected success");
  [%expect {| cancelled |}]
;;
