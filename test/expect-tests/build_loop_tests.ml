open Stdune
include Dune_scheduler
open Dune_tests_common
open Dune_engine
open Fiber.O

let () = init ()

let default =
  Clflags.display := Short;
  { Scheduler.Config.concurrency = 1
  ; print_ctrl_c_warning = false
  ; watch_exclusions = []
  }
;;

let go ?(timeout = Time.Span.of_secs 0.3) ?(config = default) f =
  try Scheduler.Run.go ~timeout config ~file_watcher:No_watcher f with
  | Shutdown.E Requested -> ()
;;

let cell = Memo.lazy_cell Memo.return

let%expect_test "cancelling a build" =
  let build_started = Fiber.Ivar.create () in
  let build_cancelled = Fiber.Ivar.create () in
  go (fun () ->
    Build_loop.run (fun build_loop ->
      Fiber.fork_and_join_unit
        (fun () ->
           Build_loop.poll build_loop (fun ~run_id:_ ~restart_started_at:_ ->
             let* () = Fiber.Ivar.fill build_started () in
             let* () = Fiber.Ivar.read build_cancelled in
             let* res =
               Fiber.collect_errors (fun () -> Scheduler.with_job_slot Fiber.return)
             in
             print_endline
               (match res with
                | Ok () -> "FAIL: build wasn't cancelled"
                | Error _ -> "PASS: build was cancelled");
             let () = Scheduler.shutdown () in
             Fiber.never))
        (fun () ->
           let* () = Fiber.Ivar.read build_started in
           let* () =
             Build_loop.For_tests.inject_memo_invalidation
               build_loop
               (Memo.Cell.invalidate cell ~reason:Unknown)
           in
           (* Wait for the scheduler to acknowledge the change *)
           let* () = Build_loop.For_tests.wait_for_build_input_change build_loop in
           Fiber.Ivar.fill build_cancelled ())));
  [%expect {| PASS: build was cancelled |}]
;;

(* CR-soon jeremiedimino: currently cancelling a build cancels not only this
   build but also all running fibers, including ones that are unrelated. *)
let%expect_test "cancelling a build: effect on other fibers" =
  let build_started = Fiber.Ivar.create () in
  go (fun () ->
    Build_loop.run (fun build_loop ->
      Fiber.fork_and_join_unit
        (fun () ->
           Build_loop.poll build_loop (fun ~run_id:_ ~restart_started_at:_ ->
             let* () = Fiber.Ivar.fill build_started () in
             Fiber.never))
        (fun () ->
           let* () = Fiber.Ivar.read build_started in
           let* () =
             Build_loop.For_tests.inject_memo_invalidation
               build_loop
               (Memo.Cell.invalidate cell ~reason:Unknown)
           in
           let* () = Build_loop.For_tests.wait_for_build_input_change build_loop in
           let+ res = Fiber.collect_errors (fun () -> Fiber.return ()) in
           print_endline
             (match res with
              | Ok () -> "PASS: we can still run things outside the build"
              | Error _ -> "FAIL: other fiber got cancelled");
           Scheduler.shutdown ())));
  [%expect {| PASS: we can still run things outside the build |}]
;;
