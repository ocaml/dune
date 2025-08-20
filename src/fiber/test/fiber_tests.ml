open Stdune
open Fiber.O
open Dyn

let printf = Printf.printf
let print_dyn dyn = Dyn.to_string dyn |> print_endline
let () = Printexc.record_backtrace false

module Scheduler = struct
  let t = Test_scheduler.create ()
  let yield () = Test_scheduler.yield t
  let run f = Test_scheduler.run t f
end

let failing_fiber () : unit Fiber.t =
  let* () = Scheduler.yield () in
  raise Exit
;;

let long_running_fiber () =
  let rec loop n =
    if n = 0
    then Fiber.return ()
    else
      let* () = Scheduler.yield () in
      loop (n - 1)
  in
  loop 10
;;

let never_fiber () = Fiber.never
let backtrace_result dyn_of_ok = Result.to_dyn dyn_of_ok (list Exn_with_backtrace.to_dyn)
let unit_result dyn_of_ok = Result.to_dyn dyn_of_ok unit

let test ?(expect_never = false) to_dyn f =
  let never_raised = ref false in
  (try Scheduler.run f |> to_dyn |> print_dyn with
   | Test_scheduler.Never -> never_raised := true);
  match !never_raised, expect_never with
  | false, false ->
    (* We don't raise in this case b/c we assume something else is being
       tested *)
    ()
  | true, true -> print_endline "[PASS] Never raised as expected"
  | false, true -> print_endline "[FAIL] expected Never to be raised but it wasn't"
  | true, false -> print_endline "[FAIL] unexpected Never raised"
;;

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

let%expect_test "collect_errors" =
  test (backtrace_result unit) (Fiber.collect_errors (fun () -> raise Exit));
  [%expect {| Error [ { exn = "Stdlib.Exit"; backtrace = "" } ] |}]
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

let%expect_test "execution context of ivars" =
  (* The point of this test it show that the execution context is restored when
     a fiber that's blocked on an ivar is resumed. This means that fiber local
     variables are visible for example*)
  let open Fiber.O in
  let ivar = Fiber.Ivar.create () in
  let run_when_filled () =
    let var = Fiber.Var.create None in
    Fiber.Var.set var (Some 42) (fun () ->
      let* peek = Fiber.Ivar.peek ivar in
      assert (peek = None);
      let* () = Fiber.Ivar.read ivar in
      let+ value = Fiber.Var.get_exn var in
      Printf.printf "var value %d\n" value)
  in
  let run = Fiber.fork_and_join_unit run_when_filled (Fiber.Ivar.fill ivar) in
  test unit run;
  [%expect
    {|
    var value 42
    () |}]
;;

let%expect_test "fiber vars are preserved across yields" =
  let var = Fiber.Var.create None in
  let fiber th () =
    let* v = Fiber.Var.get var in
    assert (v = None);
    Fiber.Var.set var (Some th) (fun () ->
      let* v = Fiber.Var.get var in
      assert (v = Some th);
      let* () = Scheduler.yield () in
      let+ v = Fiber.Var.get var in
      assert (v = Some th))
  in
  let run = Fiber.fork_and_join_unit (fiber 1) (fiber 2) in
  test unit run;
  [%expect
    {|
    () |}]
;;

let%expect_test "fill returns a fiber that executes before waiters are awoken" =
  let ivar = Fiber.Ivar.create () in
  let open Fiber.O in
  let waiters () =
    let waiter n () =
      let+ () = Fiber.Ivar.read ivar in
      Printf.printf "waiter %d resumed\n" n
    in
    Fiber.fork_and_join_unit (waiter 1) (waiter 2)
  in
  let run () =
    let* () = Scheduler.yield () in
    let+ () = Fiber.Ivar.fill ivar () in
    Printf.printf "ivar filled\n"
  in
  test unit (Fiber.fork_and_join_unit waiters run);
  [%expect
    {|
    ivar filled
    waiter 1 resumed
    waiter 2 resumed
    () |}]
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

(* Mvar tests *)

module Mvar = Fiber.Mvar

let%expect_test "created mvar is empty" =
  test
    ~expect_never:true
    opaque
    (let mvar : int Mvar.t = Mvar.create () in
     Mvar.read mvar);
  [%expect
    {|
    [PASS] Never raised as expected |}]
;;

let%expect_test "reading from written mvar consumes value" =
  test
    unit
    (let mvar = Mvar.create () in
     let value = "foo" in
     let* () = Mvar.write mvar value in
     let+ x = Mvar.read mvar in
     assert (value = x);
     print_endline "[PASS] mvar contains expected value");
  [%expect
    {|
    [PASS] mvar contains expected value
    () |}]
;;

let%expect_test "reading from empty mvar blocks" =
  test
    unit
    (let mvar = Mvar.create () in
     let value = "foo" in
     Fiber.fork_and_join_unit
       (fun () ->
          print_endline "reading mvar";
          let+ x = Mvar.read mvar in
          assert (value = x);
          print_endline "[PASS] mvar contains expected value")
       (fun () ->
          let* () = long_running_fiber () in
          print_endline "writing mvar";
          let+ () = Mvar.write mvar value in
          print_endline "written mvar"));
  [%expect
    {|
    reading mvar
    writing mvar
    written mvar
    [PASS] mvar contains expected value
    () |}]
;;

let%expect_test "writing multiple values" =
  test
    unit
    (let mvar = Mvar.create () in
     let write (n : int) : unit Fiber.t =
       Printf.printf "writing %d\n" n;
       Mvar.write mvar n
     in
     let read () =
       let+ n = Mvar.read mvar in
       Printf.printf "read %d\n" n;
       n
     in
     let rec produce n =
       let* () = write n in
       if n = 0 then Fiber.return () else produce (n - 1)
     in
     let rec consume () =
       let* n = read () in
       if n = 0 then Fiber.return () else consume ()
     in
     Fiber.fork_and_join_unit (fun () -> produce 3) consume);
  (* Writing to a mvar only blocks if the mvar is full. Similarly, reading from
     a mvar only blocks if the mvar is empty. This is why [write] and [read]
     operations in the output below are grouped two by two. *)
  [%expect
    {|
    writing 3
    writing 2
    read 3
    read 2
    writing 1
    writing 0
    read 1
    read 0
    () |}]
;;

let%expect_test "writing multiple values" =
  test
    unit
    (let m = Mvar.create () in
     Fiber.fork_and_join_unit
       (fun () ->
          print_endline "reader1: reading";
          let* x = Mvar.read m in
          printf "reader1: got %d\n" x;
          print_endline "reader1: writing";
          Mvar.write m 1)
       (fun () ->
          let* () = Scheduler.yield () in
          print_endline "reader2: writing";
          let* () = Mvar.write m 2 in
          print_endline "reader2: reading";
          let+ x = Mvar.read m in
          printf "reader2: got %d\n" x));
  [%expect
    {|
    reader1: reading
    reader2: writing
    reader2: reading
    reader1: got 2
    reader1: writing
    reader2: got 1
    () |}]
;;

let stream a b =
  let n = ref a in
  Fiber.Stream.In.create (fun () ->
    if !n > b
    then Fiber.return None
    else (
      let x = !n in
      n := x + 1;
      Fiber.return (Some x)))
;;

let%expect_test "Stream.parallel_iter is indeed parallel" =
  let test ~iter_function =
    Scheduler.run
      (iter_function (stream 1 3) ~f:(fun n ->
         Printf.printf "%d: enter\n" n;
         let* () = long_running_fiber () in
         Printf.printf "%d: leave\n" n;
         Fiber.return ()))
  in
  (* The [enter] amd [leave] messages must be interleaved to indicate that the
     calls to [f] are executed in parallel: *)
  test ~iter_function:Fiber.Stream.In.parallel_iter;
  [%expect
    {|
    1: enter
    2: enter
    3: enter
    1: leave
    2: leave
    3: leave |}];
  (* With [sequential_iter] however, The [enter] amd [leave] messages must be
     paired in sequence: *)
  test ~iter_function:Fiber.Stream.In.sequential_iter;
  [%expect
    {|
    1: enter
    1: leave
    2: enter
    2: leave
    3: enter
    3: leave |}]
;;

let%expect_test "Stream.*_iter can be finalized" =
  let test ~iter_function =
    Scheduler.run
      (Fiber.finalize
         ~finally:(fun () ->
           Printf.printf "finalized";
           Fiber.return ())
         (fun () -> iter_function (stream 1 3) ~f:(fun _ -> Fiber.return ())))
  in
  test ~iter_function:Fiber.Stream.In.sequential_iter;
  [%expect {| finalized |}];
  test ~iter_function:Fiber.Stream.In.parallel_iter;
  [%expect {| finalized |}]
;;

let rec naive_stream_parallel_iter (t : _ Fiber.Stream.In.t) ~f =
  Fiber.Stream.In.read t
  >>= function
  | None -> Fiber.return ()
  | Some x ->
    Fiber.fork_and_join_unit
      (fun () ->
         (* without this [yield], our attempt to leak memory is defeated by an
           optimization in [fork_and_join_unit]*)
         Scheduler.yield () >>= fun () -> f x)
      (fun () -> naive_stream_parallel_iter t ~f)
;;

let%expect_test "Stream.parallel_iter doesn't leak" =
  (* Check that a naive [parallel_iter] functions on streams is leaking memory,
     while [Fiber.Stream.parallel_iter] does not. To do that, we construct a
     long stream and iterate over it. At each iteration, we do a full major GC
     and count the number of live words. With the naive implementation, we check
     that this number increases while with the right one we check that this
     number is constant.

     This test is carefully crafted to avoid creating new live words as we
     iterate through the stream. As a result, the only new live words that can
     appear are because of the iteration function. *)
  let test n ~iter_function =
    let finish_stream = Fiber.Ivar.create () in
    let stream =
      let count = ref n in
      Fiber.Stream.In.create (fun () ->
        if !count > 0
        then (
          decr count;
          Fiber.return (Some ()))
        else
          let* () = Fiber.Ivar.read finish_stream in
          Fiber.return None)
    in
    let awaiting = ref n in
    let iter_await = Fiber.Ivar.create () in
    let record () =
      Gc.full_major ();
      let curr = (Gc.stat ()).live_words in
      curr
    in
    let f () =
      decr awaiting;
      if !awaiting = 0 then Fiber.Ivar.fill iter_await () else Fiber.return ()
    in
    Scheduler.run
      (let+ prev, curr =
         Fiber.fork_and_join
           (fun () ->
              let prev = record () in
              let+ () = iter_function stream ~f in
              prev)
           (fun () ->
              let* () = Fiber.Ivar.read iter_await in
              let curr = record () in
              let+ () = Fiber.Ivar.fill finish_stream () in
              curr)
       in
       curr - prev)
  in
  let data_points = [ 1; 10; 100; 1000 ] in
  let rec pair_wise_check ~f = function
    | [] | [ _ ] -> true
    | x :: y :: xs -> f x y && pair_wise_check ~f (y :: xs)
  in
  let test ~pred ~iter_function =
    let results = List.map data_points ~f:(test ~iter_function) in
    if pair_wise_check results ~f:pred
    then print_endline "[PASS] memory usage is as expected"
    else (
      print_endline "[FAIL] memory usage is not as expected";
      Dyn.(list int) results |> print_dyn)
  in
  (* Check that the number of live words keeps on increasing because we are
     leaking memory: *)
  test ~pred:( < ) ~iter_function:naive_stream_parallel_iter;
  [%expect {| [PASS] memory usage is as expected |}];
  test ~pred:( = ) ~iter_function:Fiber.Stream.In.parallel_iter;
  [%expect {| [PASS] memory usage is as expected |}]
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

let%expect_test "Stream: multiple readers is an error" =
  (* [stream] is so that the first element takes longer to be produced. An
     implementation supporting multiple readers should still yield the first
     element before the second. *)
  let stream =
    let n = ref 0 in
    Fiber.Stream.In.create (fun () ->
      let x = !n in
      n := x + 1;
      let+ () =
        if x = 0
        then
          let* () = long_running_fiber () in
          long_running_fiber ()
        else Fiber.return ()
      in
      Some ())
  in
  Scheduler.run
    (Fiber.fork_and_join_unit
       (fun () ->
          printf "Reader 1 reading\n";
          let+ _x = Fiber.Stream.In.read stream in
          printf "Reader 1 done\n")
       (fun () ->
          let* () = long_running_fiber () in
          printf "Reader 2 reading\n";
          let+ _x = Fiber.Stream.In.read stream in
          printf "Reader 2 done\n"))
[@@expect.uncaught_exn
  {|
  ("(\"Fiber.Stream.In: already reading\", {})")
  Trailing output
  ---------------
  Reader 1 reading
  Reader 2 reading |}]
;;

let%expect_test "Stream: multiple writers is an error" =
  (* [stream] is so that the first element takes longer to be consumed. An
     implementation supporting multiple writers should still yield the first
     element before the second. *)
  let stream =
    Fiber.Stream.Out.create (function
      | Some 1 ->
        let* () = long_running_fiber () in
        long_running_fiber ()
      | _ -> Fiber.return ())
  in
  Scheduler.run
    (Fiber.fork_and_join_unit
       (fun () ->
          printf "Writer 1 writing\n";
          let+ _x = Fiber.Stream.Out.write stream (Some 1) in
          printf "Writer 1 done\n")
       (fun () ->
          let* () = long_running_fiber () in
          printf "Writer 2 writing\n";
          let+ _x = Fiber.Stream.Out.write stream (Some 2) in
          printf "Writer 2 done\n"))
[@@expect.uncaught_exn
  {|
  ("(\"Fiber.Stream.Out: already writing\", {})")
  Trailing output
  ---------------
  Writer 1 writing
  Writer 2 writing |}]
;;

let%expect_test "Stream: writing on a closed stream is an error" =
  Scheduler.run
    (let out =
       Fiber.Stream.Out.create (fun x ->
         print_dyn ((option unit) x);
         Fiber.return ())
     in
     let* () = Fiber.Stream.Out.write out None in
     Fiber.Stream.Out.write out (Some ()))
[@@expect.uncaught_exn
  {|
  ("(\"Fiber.Stream.Out: stream output closed\", {})")
  Trailing output
  ---------------
  None |}]
;;

module Pool = Fiber.Pool

let%expect_test "start & stop pool" =
  Scheduler.run
    (let pool = Pool.create () in
     Pool.close pool);
  [%expect {| |}]
;;

let%expect_test "run 2 tasks" =
  Scheduler.run
    (let pool = Pool.create () in
     let task n () =
       printf "task %d\n" n;
       Fiber.return ()
     in
     let tasks () =
       Fiber.parallel_iter [ 1; 2 ] ~f:(fun n -> Pool.task pool ~f:(task n))
     in
     Fiber.fork_and_join_unit
       (fun () -> Pool.run pool)
       (fun () ->
          let* () = tasks () in
          Pool.close pool));
  [%expect
    {|
    task 1
    task 2 |}]
;;

let%expect_test "raise exception" =
  Scheduler.run
    (let pool = Pool.create () in
     let* () = Pool.task pool ~f:(fun () -> raise Exit) in
     Fiber.fork_and_join_unit
       (fun () ->
          let+ res = Fiber.collect_errors (fun () -> Pool.run pool) in
          match res with
          | Ok _ -> assert false
          | Error [ e ] ->
            assert (e.exn = Exit);
            print_endline "Caught Exit"
          | _ -> assert false)
       (fun () -> Pool.close pool));
  [%expect {| Caught Exit |}]
;;

let%expect_test "double run a pool" =
  (* Calling [Pool.run] twice on the same pool shouldn't be allowed

     We can't allow competing [Pool.run] calls to get tasks. We want to make
     sure only a single [run] will get the exceptions from all tasks in the
     pool *)
  (Scheduler.run
   @@
   let pool = Pool.create () in
   Fiber.fork_and_join_unit (fun () -> Pool.run pool) (fun () -> Pool.run pool));
  [%expect.unreachable]
[@@expect.uncaught_exn
  {| ("(\"Fiber.Pool.run: concurent calls to run aren't allowed\", {})") |}]
;;

let%expect_test "run -> stop -> run a pool" =
  (* We shouldn't be able to call [Pool.run] again after we already called
     [Pool.run] and [Pool.close]. In other words, we can't reuse pools *)
  (Scheduler.run
   @@
   let pool = Pool.create () in
   let* () =
     Fiber.fork_and_join_unit
       (fun () -> Pool.run pool)
       (fun () -> Fiber.Pool.task pool ~f:(fun () -> Pool.close pool))
   in
   Pool.run pool);
  [%expect.unreachable]
[@@expect.uncaught_exn
  {| ("(\"Fiber.Pool.run: concurent calls to run aren't allowed\", {})") |}]
;;

let%expect_test "stop a pool and then run it" =
  (Scheduler.run
   @@
   let pool = Pool.create () in
   let* () = Pool.close pool in
   Pool.run pool);
  [%expect {||}]
;;

let%expect_test "pool - weird deadlock" =
  (* this doesn't dead lock *)
  (Scheduler.run
   @@
   let pool = Pool.create () in
   let* () = Pool.task pool ~f:Fiber.return in
   Fiber.fork_and_join_unit (fun () -> Pool.close pool) (fun () -> Pool.run pool));
  [%expect {||}];
  (* but this does *)
  (Scheduler.run
   @@
   let pool = Pool.create () in
   let* () = Pool.task pool ~f:Fiber.return in
   let* () = Pool.task pool ~f:Fiber.return in
   Fiber.fork_and_join_unit (fun () -> Pool.close pool) (fun () -> Pool.run pool));
  [%expect {||}]
;;

let%expect_test "nested run in task" =
  (Scheduler.run
   @@
   let pool = Pool.create () in
   let* () = Pool.task pool ~f:(fun () -> Pool.run pool) in
   Fiber.fork_and_join_unit (fun () -> Pool.close pool) (fun () -> Pool.run pool));
  [%expect.unreachable]
[@@expect.uncaught_exn
  {| ("(\"Fiber.Pool.run: concurent calls to run aren't allowed\", {})") |}]
;;

let%expect_test "nested tasks" =
  (Scheduler.run
   @@
   let pool = Pool.create () in
   let* () =
     Pool.task pool ~f:(fun () ->
       print_endline "outer";
       let* () =
         Pool.task pool ~f:(fun () ->
           let+ () = Fiber.return () in
           print_endline "inner")
       in
       Pool.close pool)
   in
   Pool.run pool);
  [%expect
    {|
    outer
    inner |}]
;;

let%expect_test "stopping inside a task" =
  (Scheduler.run
   @@
   let pool = Pool.create () in
   let* () = Pool.task pool ~f:(fun () -> Pool.close pool) in
   Pool.run pool);
  [%expect {||}]
;;

let%expect_test "stack usage with consecutive Ivar.fill" =
  let stack_size () = (Gc.stat ()).stack_size in
  let rec loop acc prev n =
    if n = 0
    then acc, prev
    else (
      let next = Fiber.Ivar.create () in
      let fiber =
        let* () = Fiber.Ivar.read prev in
        Fiber.Ivar.fill next ()
      in
      loop (fiber :: acc) next (n - 1))
  in
  let stack_usage n =
    let first = Fiber.Ivar.create () in
    let fibers, final = loop [] first n in
    let* () = Fiber.parallel_iter fibers ~f:Fun.id
    and* n =
      let init = stack_size () in
      let+ () = Fiber.Ivar.read final in
      stack_size () - init
    and* () = Fiber.Ivar.fill first () in
    Fiber.return n
  in
  let n0 = Scheduler.run (stack_usage 0) in
  let n1000 = Scheduler.run (stack_usage 1000) in
  if n0 = n1000
  then printf "[PASS]"
  else
    printf
      "[FAIL]\nStack usage for n = 0:    %d words\nStack usage for n = 1000: %d words\n"
      n0
      n1000;
  [%expect
    {|
    [PASS] |}]
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

let%expect_test "cancel_test1" =
  let cancel = Fiber.Cancel.create () in
  Scheduler.run
    (printf "%B\n" (Fiber.Cancel.fired cancel);
     let* () = Fiber.Cancel.fire cancel in
     printf "%B\n" (Fiber.Cancel.fired cancel);
     Fiber.return ());
  [%expect
    {|
    false
    true |}]
;;

let%expect_test "cancel_test2" =
  let cancel = Fiber.Cancel.create () in
  let ivar1 = Fiber.Ivar.create () in
  let ivar2 = Fiber.Ivar.create () in
  let (), what =
    Scheduler.run
      (Fiber.Cancel.with_handler
         cancel
         (fun () ->
            let* () = Fiber.Ivar.fill ivar1 () in
            let* () = Fiber.Cancel.fire cancel in
            Fiber.Ivar.read ivar2)
         ~on_cancel:(fun () -> Fiber.Ivar.fill ivar2 ()))
  in
  print_endline
    (match what with
     | Cancelled () -> "PASS"
     | Not_cancelled -> "FAIL");
  [%expect
    {|
    PASS |}]
;;

let%expect_test "cancel_test3" =
  let cancel = Fiber.Cancel.create () in
  let (), what =
    Scheduler.run
      (Fiber.Cancel.with_handler
         cancel
         (fun () -> Fiber.return ())
         ~on_cancel:(fun () -> assert false))
  in
  print_endline
    (match what with
     | Cancelled () -> "FAIL"
     | Not_cancelled -> "PASS");
  [%expect
    {|
    PASS |}]
;;

let%expect_test "cancel_test4" =
  let cancel = Fiber.Cancel.create () in
  let (), what =
    Scheduler.run
      (let* () = Fiber.Cancel.fire cancel in
       Fiber.Cancel.with_handler
         cancel
         (fun () -> Fiber.return ())
         ~on_cancel:(fun () -> Fiber.return ()))
  in
  print_endline
    (match what with
     | Cancelled () -> "PASS"
     | Not_cancelled -> "FAIL");
  [%expect {| PASS |}]
;;

let%expect_test "svar" =
  let module Svar = Fiber.Svar in
  let run () =
    let svar = Svar.create 10 in
    printfn "read: %d" (Svar.read svar);
    let* () = Svar.write svar (Svar.read svar + 1) in
    let* () = Svar.write svar (Svar.read svar + 1) in
    printfn "read: %d" (Svar.read svar);
    Fiber.fork_and_join_unit
      (fun () ->
         printfn "waiter: waiting for value > 15";
         let+ () = Svar.wait svar ~until:(fun x -> x > 15) in
         printfn "wait: %d" (Svar.read svar))
      (fun () ->
         printfn "setter: modifying value to 17";
         Svar.write svar 17)
  in
  Scheduler.run (Fiber.of_thunk run);
  [%expect
    {|
    read: 10
    read: 12
    waiter: waiting for value > 15
    setter: modifying value to 17
    wait: 17 |}]
;;
