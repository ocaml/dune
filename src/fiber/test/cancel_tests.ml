open Fiber.O
open Common

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
