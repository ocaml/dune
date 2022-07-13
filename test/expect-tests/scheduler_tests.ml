open Stdune
open! Dune_tests_common
open Dune_engine
open Fiber.O

let go f =
  let config =
    { Scheduler.Config.concurrency = 1
    ; display = { verbosity = Short; status_line = false }
    ; stats = None
    }
  in
  try
    Scheduler.Run.go config ~file_watcher:No_watcher ~on_event:(fun _ _ -> ()) f
  with Scheduler.Run.Shutdown.E Requested -> ()

let true_ = Bin.which "true" ~path:(Env.path Env.initial) |> Option.value_exn

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
            Scheduler.inject_memo_invalidation
              (Memo.Cell.invalidate cell ~reason:Unknown)
          in
          (* Wait for the scheduler to acknowledge the change *)
          let* () = Scheduler.wait_for_build_input_change () in
          Fiber.Ivar.fill build_cancelled ()));
  [%expect {| PASS: build was cancelled |}]

(* CR-soon jeremiedimino: currently cancelling a build cancels not only this
   build but also all runing fibers, including ones that are unrelated. *)
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
            Scheduler.inject_memo_invalidation
              (Memo.Cell.invalidate cell ~reason:Unknown)
          in
          let* () = Scheduler.wait_for_build_input_change () in
          let* res =
            Fiber.collect_errors (fun () ->
                Scheduler.with_job_slot (fun _ _ -> Fiber.return ()))
          in
          print_endline
            (match res with
            | Ok () -> "PASS: we can still run things outside the build"
            | Error _ -> "FAIL: other fiber got cancelled");
          Scheduler.shutdown ()));
  [%expect {| PASS: we can still run things outside the build |}]

let%expect_test "empty invalidation wakes up waiter" =
  ( go @@ fun () ->
    let await_invalidation () =
      print_endline "awaiting invalidation";
      let+ () = Scheduler.wait_for_build_input_change () in
      print_endline "awaited invalidation"
    in
    Fiber.fork_and_join_unit
      (fun () -> Scheduler.inject_memo_invalidation Memo.Invalidation.empty)
      await_invalidation );
  [%expect {|
    awaiting invalidation
    awaited invalidation |}]
