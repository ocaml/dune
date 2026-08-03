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
