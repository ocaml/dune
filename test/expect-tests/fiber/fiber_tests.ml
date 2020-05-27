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
    Queue.push ivar suspended;
    Fiber.Ivar.read ivar

  let rec restart_suspended () =
    if Queue.is_empty suspended then
      Fiber.return ()
    else
      let* () = Fiber.Ivar.fill (Queue.pop suspended) () in
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

let%expect_test _ =
  Scheduler.run (Fiber.collect_errors failing_fiber)
  |> backtrace_result unit |> print_dyn;
  [%expect {|
Error [ { exn = "Exit"; backtrace = "" } ]
|}]

let%expect_test _ =
  ( try
      ignore
        ( Scheduler.run (Fiber.collect_errors never_fiber)
          : (unit, Exn_with_backtrace.t list) Result.t );
      Result.Error "should not reach here"
    with Scheduler.Never -> Result.ok () )
  |> Result.to_dyn unit string |> print_dyn;
  [%expect {|
Ok ()
|}]

let test to_dyn f = Scheduler.run f |> to_dyn |> print_dyn

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
  test opaque
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
  [%expect.unreachable]
  [@@expect.uncaught_exn
    {|
  (Dune_fiber_tests.Fiber_tests.Scheduler.Never)
  Trailing output
  ---------------
  outer: raised Exit
  inner: raised Exit
  outer: raised Exit |}]

(* Collect errors has a subtle behavior. It can cause a fiber not to terminate
   if all the sub-fibers spawned aren't awaited *)
let%expect_test "collect_errors and termination" =
  let fiber =
    Fiber.fork_and_join_unit long_running_fiber (fun () ->
        Fiber.collect_errors (fun () ->
            let* (_ : unit Fiber.Future.t) = Fiber.fork Fiber.return in
            Fiber.return 50))
  in
  test (backtrace_result int) fiber
  [@@expect.uncaught_exn {| (Dune_fiber_tests.Fiber_tests.Scheduler.Never) |}]

let flag_set = ref false

let never_raised = ref false

let%expect_test _ =
  ( try
      Scheduler.run
        (Fiber.fork_and_join_unit never_fiber (fun () ->
             Fiber.collect_errors failing_fiber >>= fun _ ->
             long_running_fiber () >>= fun () -> Fiber.return (flag_set := true)))
    with Scheduler.Never -> never_raised := true );
  [%expect {| |}]

let%expect_test _ =
  (!flag_set && !never_raised) |> bool |> print_dyn;
  [%expect {|
true
|}]

let flag_set = ref false

let never_raised = ref false

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
  ( try
      Scheduler.run
        (Fiber.fork_and_join_unit never_fiber (fun () ->
             Fiber.collect_errors forking_fiber >>= fun _ ->
             long_running_fiber () >>= fun () -> Fiber.return (flag_set := true)))
    with Scheduler.Never -> never_raised := true )
  |> unit |> print_dyn;
  [%expect {|
()
|}]

let%expect_test _ =
  (!flag_set && !never_raised) |> bool |> print_dyn;
  [%expect {|
true
|}]
