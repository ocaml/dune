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

let%expect_test "cancelling parent triggers child cancellation" =
  let run () =
    let parent = Fiber.Cancel.create () in
    let child = Fiber.Cancel.make_child parent in
    let parent_cancelled = Fiber.Ivar.create () in
    let child_running = Fiber.Ivar.create () in
    let child_cancelled = Fiber.Ivar.create () in
    let+ (), res =
      Fiber.Cancel.with_handler
        parent
        ~on_cancel:(fun () ->
          print_endline "parent cancel";
          Fiber.Ivar.fill parent_cancelled ())
        (fun () ->
          print_endline "with parent";
          Fiber.fork_and_join_unit
            (fun () ->
              let+ (), res =
                Fiber.Cancel.with_handler
                  child
                  ~on_cancel:(Fiber.Ivar.fill child_cancelled)
                  (fun () ->
                    let* () = Fiber.Ivar.fill child_running () in
                    Fiber.Ivar.read child_cancelled)
              in
              match res with
              | Not_cancelled -> assert false
              | Cancelled () -> print_endline "child cancelled")
            (fun () ->
              let* () = Fiber.Ivar.read child_running in
              let* () = Fiber.Cancel.fire parent in
              Fiber.Ivar.read parent_cancelled))
    in
    match res with
    | Not_cancelled -> assert false
    | Cancelled () -> print_endline "parent cancelled"
  in
  Scheduler.run (Fiber.of_thunk run);
  [%expect
    {|
    with parent
    parent cancel
    child cancelled
    parent cancelled
    |}]
;;

let%expect_test "child of cancelled" =
  let run () =
    let parent = Fiber.Cancel.create () in
    let+ () = Fiber.Cancel.fire parent in
    let child = Fiber.Cancel.make_child parent in
    printf "child should be cancelled: %b" (Fiber.Cancel.fired child)
  in
  Scheduler.run (Fiber.of_thunk run);
  [%expect {| child should be cancelled: true |}]
;;

let%expect_test "cancelling child does not cancel parent" =
  let run () =
    let parent = Fiber.Cancel.create () in
    let child = Fiber.Cancel.make_child parent in
    let+ (), res =
      Fiber.Cancel.with_handler
        parent
        ~on_cancel:(fun () ->
          print_endline "cancelling parent";
          Fiber.return ())
        (fun () ->
          let* (), res =
            Fiber.Cancel.with_handler
              child
              ~on_cancel:(fun () ->
                print_endline "cancelling child";
                Fiber.return ())
              (fun () -> Fiber.Cancel.fire child)
          in
          match res with
          | Not_cancelled -> assert false
          | Cancelled () ->
            print_endline "child cancelled";
            Fiber.Cancel.fire parent)
    in
    match res with
    | Not_cancelled -> assert false
    | Cancelled () -> print_endline "parent cancelled"
  in
  Scheduler.run (Fiber.of_thunk run);
  [%expect
    {|
    cancelling child
    child cancelled
    cancelling parent
    parent cancelled
    |}]
;;
