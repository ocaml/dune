open Stdune
open! Dune_tests_common
open Dune_engine
open Fiber.O

let () = init ()

let default =
  Clflags.display := Short;
  { Scheduler.Config.concurrency = 1
  ; stats = None
  ; print_ctrl_c_warning = false
  ; watch_exclusions = []
  }
;;

let go ?(timeout = 0.3) ?(config = default) f =
  try
    Scheduler.Run.go ~timeout config ~file_watcher:No_watcher ~on_event:(fun _ _ -> ()) f
  with
  | Scheduler.Run.Shutdown.E Requested -> ()
;;

let true_ = Bin.which "true" ~path:(Env_path.path Env.initial) |> Option.value_exn
let cell = Memo.lazy_cell Memo.return

let%expect_test "cancelling a build" =
  let build_started = Fiber.Ivar.create () in
  let build_cancelled = Fiber.Ivar.create () in
  go (fun () ->
    Fiber.fork_and_join_unit
      (fun () ->
        Scheduler.Run.poll
          (let* () = Fiber.Ivar.fill build_started () in
           let* () = Fiber.Ivar.read build_cancelled in
           let* res =
             Fiber.collect_errors (fun () ->
               Scheduler.with_job_slot (fun _ _ -> Fiber.return ()))
           in
           print_endline
             (match res with
              | Ok () -> "FAIL: build wasn't cancelled"
              | Error _ -> "PASS: build was cancelled");
           let* () = Scheduler.shutdown () in
           Fiber.never))
      (fun () ->
        let* () = Fiber.Ivar.read build_started in
        let* () =
          Scheduler.inject_memo_invalidation (Memo.Cell.invalidate cell ~reason:Unknown)
        in
        (* Wait for the scheduler to acknowledge the change *)
        let* () = Scheduler.wait_for_build_input_change () in
        Fiber.Ivar.fill build_cancelled ()));
  [%expect {| PASS: build was cancelled |}]
;;

(* CR-soon jeremiedimino: currently cancelling a build cancels not only this
   build but also all running fibers, including ones that are unrelated. *)
let%expect_test "cancelling a build: effect on other fibers" =
  let build_started = Fiber.Ivar.create () in
  go (fun () ->
    Fiber.fork_and_join_unit
      (fun () ->
        Scheduler.Run.poll
          (let* () = Fiber.Ivar.fill build_started () in
           Fiber.never))
      (fun () ->
        let* () = Fiber.Ivar.read build_started in
        let* () =
          Scheduler.inject_memo_invalidation (Memo.Cell.invalidate cell ~reason:Unknown)
        in
        let* () = Scheduler.wait_for_build_input_change () in
        let* res = Fiber.collect_errors (fun () -> Fiber.return ()) in
        print_endline
          (match res with
           | Ok () -> "PASS: we can still run things outside the build"
           | Error _ -> "FAIL: other fiber got cancelled");
        Scheduler.shutdown ()));
  [%expect {| PASS: we can still run things outside the build |}]
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
