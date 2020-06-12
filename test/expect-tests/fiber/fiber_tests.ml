open Dune
open Stdune
open Fiber.O
open Dyn.Encoder
open Dune_tests_common

let () = init ()

module Scheduler : sig
  exception Never

  val yield : unit -> unit Fiber.t

  val run : 'a Fiber.t -> 'a
end = struct
  let suspended = Queue.create ()

  let yield () =
    let ivar = Fiber.Ivar.create () in
    Queue.push suspended ivar;
    Fiber.Ivar.read ivar

  let rec restart_suspended () =
    match Queue.pop suspended with
    | None -> Fiber.return ()
    | Some e ->
      let* () = Fiber.Ivar.fill e () in
      restart_suspended ()

  exception Never

  let run t =
    match
      Fiber.run
        (let* result = Fiber.fork (fun () -> t) in
         let* () = restart_suspended () in
         Fiber.Future.peek result)
    with
    | None
    | Some None ->
      raise Never
    | Some (Some x) -> x
end

let failing_fiber () : unit Fiber.t =
  Scheduler.yield () >>= fun () -> raise Exit

let long_running_fiber () =
  let rec loop n =
    if n = 0 then
      Fiber.return ()
    else
      Scheduler.yield () >>= fun () -> loop (n - 1)
  in
  loop 10

let never_fiber () = Fiber.never

let backtrace_result dyn_of_ok =
  Result.to_dyn dyn_of_ok (list Exn_with_backtrace.to_dyn)

let test ?(expect_never = false) to_dyn f =
  let never_raised = ref false in
  ( try Scheduler.run f |> to_dyn |> print_dyn
    with Scheduler.Never -> never_raised := true );
  match (!never_raised, expect_never) with
  | false, false ->
    (* We don't raise in this case b/c we assume something else is being tested *)
    ()
  | true, true -> print_endline "[PASS] Never raised as expected"
  | false, true ->
    print_endline "[FAIL] expected Never to be raised but it wasn't"
  | true, false -> print_endline "[FAIL] unexpected Never raised"

let%expect_test "execution context of ivars" =
  (* The point of this test it show that the execution context is restored when
     a fiber that's blocked on an ivar is resumed. This means that fiber local
     variables are visible for exmaple*)
  let open Fiber.O in
  let ivar = Fiber.Ivar.create () in
  let run_when_filled () =
    let var = Fiber.Var.create () in
    Fiber.Var.set var 42 (fun () ->
        let* peek = Fiber.Ivar.peek ivar in
        assert (peek = None);
        let+ () = Fiber.Ivar.read ivar in
        let value = Fiber.Var.get_exn var in
        Printf.printf "var value %d\n" value)
  in
  let run = Fiber.fork_and_join_unit run_when_filled (Fiber.Ivar.fill ivar) in
  test unit run;
  [%expect {|
    var value 42
    () |}]

let%expect_test "fiber vars are preseved across yields" =
  let var = Fiber.Var.create () in
  let fiber th () =
    assert (Fiber.Var.get var = None);
    Fiber.Var.set var th (fun () ->
        assert (Fiber.Var.get var = Some th);
        let+ () = Scheduler.yield () in
        assert (Fiber.Var.get var = Some th))
  in
  let run = Fiber.fork_and_join_unit (fiber 1) (fiber 2) in
  test unit run;
  [%expect {|
    () |}]

let%expect_test "fill returns a fiber that executes when waiters finish" =
  let ivar = Fiber.Ivar.create () in
  let open Fiber.O in
  let waiters () =
    let waiter n () =
      let+ () = Fiber.Ivar.read ivar in
      Format.eprintf "waiter %d finished running@.%!" n
    in
    Fiber.fork_and_join_unit (waiter 1) (waiter 2)
  in
  let run () =
    let* () = Scheduler.yield () in
    let+ () = Fiber.Ivar.fill ivar () in
    Format.eprintf "waiters finished running@."
  in
  test unit (Fiber.fork_and_join_unit waiters run);
  [%expect
    {|
    waiter 1 finished running
    waiter 2 finished running
    waiters finished running
    () |}]

let%expect_test _ =
  test (backtrace_result unit) (Fiber.collect_errors failing_fiber);
  [%expect {|
Error [ { exn = "Exit"; backtrace = "" } ]
|}]

let%expect_test _ =
  test ~expect_never:true opaque (Fiber.collect_errors never_fiber);
  [%expect {|
[PASS] Never raised as expected
|}]

let%expect_test _ =
  test (backtrace_result unit)
    (Fiber.collect_errors (fun () ->
         failing_fiber () >>= fun () -> failing_fiber ()));
  [%expect {|
Error [ { exn = "Exit"; backtrace = "" } ]
|}]

let log_error (e : Exn_with_backtrace.t) =
  Printf.printf "raised %s\n" (Printexc.to_string e.exn)

let%expect_test _ =
  test (backtrace_result unit)
    (Fiber.collect_errors (fun () ->
         Fiber.with_error_handler failing_fiber ~on_error:log_error));
  [%expect {|
raised Exit
Error []
|}]

let%expect_test _ =
  test
    (backtrace_result (pair unit unit))
    (Fiber.collect_errors (fun () ->
         Fiber.fork_and_join failing_fiber long_running_fiber));
  [%expect {|
Error [ { exn = "Exit"; backtrace = "" } ]
|}]

let%expect_test _ =
  test
    (pair (backtrace_result unit) unit)
    (Fiber.fork_and_join
       (fun () -> Fiber.collect_errors failing_fiber)
       long_running_fiber);
  [%expect {|
(Error [ { exn = "Exit"; backtrace = "" } ], ())
|}]

let%expect_test _ =
  test ~expect_never:true opaque
    (Fiber.fork_and_join
       (fun () ->
         let log_error by (e : Exn_with_backtrace.t) =
           Printf.printf "%s: raised %s\n" by (Printexc.to_string e.exn)
         in
         Fiber.with_error_handler ~on_error:(log_error "outer") (fun () ->
             Fiber.fork_and_join failing_fiber (fun () ->
                 Fiber.with_error_handler
                   ~on_error:(fun e ->
                     log_error "inner" e;
                     raise Exit)
                   failing_fiber)))
       long_running_fiber);
  [%expect
    {|
    outer: raised Exit
    inner: raised Exit
    outer: raised Exit
    [PASS] Never raised as expected |}]

(* Collect errors has a subtle behavior. It can cause a fiber not to terminate
   if all the sub-fibers spawned aren't awaited *)
let%expect_test "collect_errors and termination" =
  let fiber =
    Fiber.fork_and_join_unit long_running_fiber (fun () ->
        Fiber.collect_errors (fun () ->
            let* (_ : unit Fiber.Future.t) = Fiber.fork Fiber.return in
            Fiber.return 50))
  in
  test ~expect_never:true (backtrace_result int) fiber;
  [%expect {| [PASS] Never raised as expected |}]

let must_set_flag f =
  let flag = ref false in
  let setter () = flag := true in
  let check_set () =
    print_endline
      ( if !flag then
        "[PASS] flag set"
      else
        "[FAIL] flag not set" )
  in
  try
    f setter;
    check_set ()
  with e ->
    check_set ();
    raise e

let%expect_test _ =
  must_set_flag (fun setter ->
      test ~expect_never:true unit
      @@ Fiber.fork_and_join_unit never_fiber (fun () ->
             Fiber.collect_errors failing_fiber >>= fun res ->
             print_dyn (backtrace_result unit res);
             long_running_fiber () >>= fun () -> Fiber.return (setter ())));
  [%expect
    {|
    Error [ { exn = "Exit"; backtrace = "" } ]
    [PASS] Never raised as expected
    [PASS] flag set |}]

let%expect_test _ =
  let forking_fiber () =
    let which = Bin.which ~path:(Env.path Env.initial) in
    Fiber.parallel_map [ 1; 2; 3; 4; 5 ] ~f:(fun x ->
        Scheduler.yield () >>= fun () ->
        if x mod 2 = 1 then
          Process.run Process.Strict ~env:Env.initial
            (Option.value_exn (which "true"))
            []
        else
          Process.run Process.Strict ~env:Env.initial
            (Option.value_exn (which "false"))
            [])
  in
  must_set_flag (fun setter ->
      test ~expect_never:true unit
      @@ Fiber.fork_and_join_unit never_fiber (fun () ->
             Fiber.collect_errors forking_fiber >>= fun res ->
             print_dyn (backtrace_result (list unit) res);
             long_running_fiber () >>= fun () -> Fiber.return (setter ())));
  [%expect
    {|
    Error
      [ { exn = "(Failure Univ_map.find_exn)"; backtrace = "" }
      ; { exn = "(Failure Univ_map.find_exn)"; backtrace = "" }
      ; { exn = "(Failure Univ_map.find_exn)"; backtrace = "" }
      ; { exn = "(Failure Univ_map.find_exn)"; backtrace = "" }
      ; { exn = "(Failure Univ_map.find_exn)"; backtrace = "" }
      ]
    [PASS] Never raised as expected
    [PASS] flag set |}]

let%expect_test "finalize/fork behavior" =
  let fiber =
    let log s = Fiber.return (print_endline s) in
    let open Fiber.O in
    Fiber.finalize
      ~finally:(fun () -> log "finally")
      (fun () ->
        let* f = Fiber.fork (fun () -> log "fork") in
        let* () = log "after fork" in
        let* () = Fiber.Future.wait f in
        log "fiber finished")
  in
  test unit fiber;
  [%expect
    {|
    fork
    after fork
    fiber finished
    finally
    () |}]
