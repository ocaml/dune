open Dyn
open Common

let%expect_test "trigger is idempotent" =
  let trigger = Fiber.Trigger.create () in
  let open Fiber.O in
  let waiters () =
    let waiter n () =
      let+ () = Fiber.Trigger.wait trigger in
      Printf.printf "waiter %d resumed\n" n
    in
    Fiber.fork_and_join_unit (waiter 1) (waiter 2)
  in
  let run () =
    let* () = Scheduler.yield () in
    let* () = Fiber.Trigger.trigger trigger in
    Printf.printf "triggered once\n";
    let* () = Fiber.Trigger.trigger trigger in
    Printf.printf "triggered twice\n";
    let+ () = Fiber.Trigger.wait trigger in
    Printf.printf "late waiter resumed\n"
  in
  test unit (Fiber.fork_and_join_unit waiters run);
  [%expect
    {|
    triggered once
    triggered twice
    late waiter resumed
    waiter 1 resumed
    waiter 2 resumed
    () |}]
;;
