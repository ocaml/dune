open Stdune
open Fiber.O

let () = Dune_tests_common.init ()

let push_log = function
  | `Ok -> printfn "Successful push."
  | `Closed -> printfn "Couldn't push! Bus was closed."
;;

let pop_log = function
  | `Next a -> printfn "Popped %S." a
  | `Closed -> printfn "Couldn't pop! Bus was closed."
;;

let push t s =
  let+ r = Fiber_event_bus.push t s in
  push_log r
;;

let pop t =
  let+ r = Fiber_event_bus.pop t in
  pop_log r
;;

let create () =
  let bus = Fiber_event_bus.create () in
  printfn "Created bus.";
  bus
;;

let close t =
  let+ () = Fiber_event_bus.close t in
  printfn "Closed bus."
;;

let test f =
  let scheduler = Test_scheduler.create () in
  let exec = Fiber.of_thunk (fun () -> f scheduler) in
  Test_scheduler.run scheduler exec
;;

let%expect_test "Push followed by pop and then close" =
  test (fun _scheduler ->
    let event_bus = create () in
    let* () = push event_bus "Hello"
    and* () = pop event_bus in
    let* () = close event_bus in
    Fiber.return ());
  [%expect
    {|
    Created bus.
    Popped "Hello".
    Successful push.
    Closed bus. |}]
;;

let%expect_test "Double close" =
  test (fun _scheduler ->
    let event_bus = create () in
    let* () = close event_bus in
    let* () = close event_bus in
    Fiber.return ());
  [%expect {|
    Created bus.
    Closed bus.
    Closed bus. |}]
;;

let%expect_test "Push together with delayed close should close bus and block push." =
  test (fun scheduler ->
    let event_bus = create () in
    let* () = push event_bus "Hello"
    and* () = Test_scheduler.yield scheduler >>> close event_bus in
    Fiber.return ());
  [%expect {|
    Created bus.
    Closed bus.
    Couldn't push! Bus was closed. |}]
;;

let%expect_test "Pop together with delayed close should close bus and block pop." =
  test (fun scheduler ->
    let event_bus = create () in
    let* () = pop event_bus
    and* () = Test_scheduler.yield scheduler >>> close event_bus in
    Fiber.return ());
  [%expect {|
    Created bus.
    Closed bus.
    Couldn't pop! Bus was closed. |}]
;;

let%expect_test "2 pushes and delayed close" =
  test (fun scheduler ->
    let event_bus = create () in
    let* () = push event_bus "Hello"
    and* () = push event_bus "World!"
    and* () = Test_scheduler.yield scheduler >>> close event_bus in
    Fiber.return ());
  [%expect
    {|
    Created bus.
    Closed bus.
    Couldn't push! Bus was closed.
    Couldn't push! Bus was closed. |}]
;;

let%expect_test "2 pops and delayed close" =
  test (fun scheduler ->
    let event_bus = create () in
    let* () = pop event_bus
    and* () = pop event_bus
    and* () = Test_scheduler.yield scheduler >>> close event_bus in
    Fiber.return ());
  [%expect
    {|
    Created bus.
    Closed bus.
    Couldn't pop! Bus was closed.
    Couldn't pop! Bus was closed. |}]
;;

let%expect_test "Push and pop with a delayed close" =
  test (fun scheduler ->
    let event_bus = create () in
    let* () = push event_bus "Hello"
    and* () = pop event_bus
    and* () = Test_scheduler.yield scheduler >>> close event_bus in
    Fiber.return ());
  [%expect
    {|
    Created bus.
    Popped "Hello".
    Successful push.
    Closed bus. |}]
;;

let%expect_test "2 pushes together with 2 pops then a close" =
  test (fun _scheduler ->
    let event_bus = create () in
    let* () = push event_bus "Hello"
    and* () = push event_bus "World!"
    and* () = pop event_bus
    and* () = pop event_bus in
    let* () = close event_bus in
    Fiber.return ());
  [%expect
    {|
    Created bus.
    Popped "Hello".
    Popped "World!".
    Successful push.
    Successful push.
    Closed bus. |}]
;;
