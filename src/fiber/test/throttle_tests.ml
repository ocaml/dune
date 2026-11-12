open Stdune
open Fiber.O
open Common
module Throttle = Fiber.Throttle

let job throttle name ~size ivar =
  Throttle.run throttle ~size ~f:(fun () ->
    printf "start %s (running %d)\n" name (Throttle.running throttle);
    let+ () = Fiber.Ivar.read ivar in
    printf "stop %s\n" name)
;;

let release name ivar =
  let* () = Scheduler.yield () in
  printf "release %s\n" name;
  Fiber.Ivar.fill ivar ()
;;

let%expect_test "two jobs that each take half of the slots run together" =
  test
    Dyn.unit
    (let throttle = Throttle.create 4 in
     let a = Fiber.Ivar.create () in
     let b = Fiber.Ivar.create () in
     let c = Fiber.Ivar.create () in
     let d = Fiber.Ivar.create () in
     Fiber.all_concurrently_unit
       [ job throttle "a" ~size:2 a
       ; job throttle "b" ~size:2 b
       ; job throttle "c" ~size:1 c
       ; job throttle "d" ~size:1 d
       ; (let* () = release "a" a in
          let* () = release "b" b in
          let* () = release "c" c in
          release "d" d)
       ]);
  [%expect
    {|
    start a (running 2)
    start b (running 4)
    release a
    stop a
    start c (running 4)
    start d (running 4)
    release b
    stop b
    release c
    stop c
    release d
    stop d
    ()
    |}]
;;

let%expect_test "a job can start even when its weight exceeds the free slots" =
  test
    Dyn.unit
    (let throttle = Throttle.create 2 in
     let x = Fiber.Ivar.create () in
     let y = Fiber.Ivar.create () in
     let z = Fiber.Ivar.create () in
     Fiber.all_concurrently_unit
       [ job throttle "x" ~size:1 x
       ; job throttle "y" ~size:2 y
       ; job throttle "z" ~size:1 z
       ; (let* () = release "x" x in
          let* () = release "y" y in
          release "z" z)
       ]);
  [%expect
    {|
    start x (running 1)
    start y (running 3)
    release x
    stop x
    release y
    stop y
    start z (running 1)
    release z
    stop z
    ()
    |}]
;;

let%expect_test "queued jobs overcommit in FIFO order when a slot becomes free" =
  test
    Dyn.unit
    (let throttle = Throttle.create 4 in
     let a = Fiber.Ivar.create () in
     let b = Fiber.Ivar.create () in
     let c = Fiber.Ivar.create () in
     let d = Fiber.Ivar.create () in
     Fiber.all_concurrently_unit
       [ job throttle "a" ~size:3 a
       ; job throttle "b" ~size:1 b
       ; job throttle "c" ~size:4 c
       ; job throttle "d" ~size:1 d
       ; (let* () = release "b" b in
          let* () = release "a" a in
          let* () = release "c" c in
          release "d" d)
       ]);
  [%expect
    {|
    start a (running 3)
    start b (running 4)
    release b
    stop b
    start c (running 7)
    release a
    stop a
    release c
    stop c
    start d (running 1)
    release d
    stop d
    ()
    |}]
;;

let%expect_test "a weight larger than the size takes all of the slots" =
  test
    Dyn.unit
    (let throttle = Throttle.create 2 in
     let w = Fiber.Ivar.create () in
     let v = Fiber.Ivar.create () in
     Fiber.all_concurrently_unit
       [ job throttle "w" ~size:5 w
       ; job throttle "v" ~size:1 v
       ; (let* () = release "w" w in
          release "v" v)
       ]);
  [%expect
    {|
    start w (running 2)
    release w
    stop w
    start v (running 1)
    release v
    stop v
    ()
    |}]
;;

let%expect_test "throttle slots cancel queued acquisitions and can be reused" =
  let throttle = Fiber.Throttle.create 3 in
  let held = Fiber.Throttle.Slot.create throttle ~size:10 in
  let queued = Fiber.Throttle.Slot.create throttle ~size:2 in
  Scheduler.run
    (let* result = Fiber.Throttle.Slot.acquire held in
     printfn
       "held: %s"
       (match result with
        | `Ok -> "ok"
        | `Cancelled -> "cancelled");
     let* () =
       Fiber.fork_and_join_unit
         (fun () ->
            let+ result = Fiber.Throttle.Slot.acquire queued in
            printfn
              "queued: %s"
              (match result with
               | `Ok -> "ok"
               | `Cancelled -> "cancelled"))
         (fun () ->
            let* () = Scheduler.yield () in
            Fiber.Throttle.Slot.release queued)
     in
     printfn "running: %d" (Fiber.Throttle.running throttle);
     let* () = Fiber.Throttle.Slot.release held in
     let* result = Fiber.Throttle.Slot.acquire queued in
     printfn
       "reused: %s"
       (match result with
        | `Ok -> "ok"
        | `Cancelled -> "cancelled");
     printfn "running: %d" (Fiber.Throttle.running throttle);
     let* () = Fiber.Throttle.Slot.release queued in
     let+ () = Fiber.Throttle.Slot.release queued in
     printfn "running: %d" (Fiber.Throttle.running throttle));
  [%expect
    {|
    held: ok
    queued: cancelled
    running: 3
    reused: ok
    running: 2
    running: 0 |}]
;;

let%expect_test "weighted throttle slots share capacity with ordinary jobs" =
  let throttle = Fiber.Throttle.create 3 in
  let held = Fiber.Throttle.Slot.create throttle ~size:2 in
  let queued = Fiber.Throttle.Slot.create throttle ~size:2 in
  Scheduler.run
    (let* (_ : [ `Ok | `Cancelled ]) = Fiber.Throttle.Slot.acquire held in
     let* () =
       Fiber.fork_and_join_unit
         (fun () ->
            Fiber.Throttle.run throttle ~size:2 ~f:(fun () ->
              printfn "ordinary: %d" (Fiber.Throttle.running throttle);
              Scheduler.yield ()))
         (fun () ->
            let* result = Fiber.Throttle.Slot.acquire queued in
            printfn
              "queued: %s, running: %d"
              (match result with
               | `Ok -> "ok"
               | `Cancelled -> "cancelled")
              (Fiber.Throttle.running throttle);
            Fiber.Throttle.Slot.release queued)
     in
     let+ () = Fiber.Throttle.Slot.release held in
     printfn "released: %d" (Fiber.Throttle.running throttle));
  [%expect
    {|
    ordinary: 4
    queued: ok, running: 4
    released: 0 |}]
;;

let%expect_test "throttle releases slots after errors" =
  let throttle = Fiber.Throttle.create 1 in
  let result =
    Scheduler.run
      (Fiber.collect_errors (fun () ->
         Fiber.Throttle.run throttle ~size:1 ~f:(fun () -> raise Exit)))
  in
  (match result with
   | Error [ { exn = Exit; _ } ] -> print_endline "caught Exit"
   | Ok () | Error _ -> print_endline "unexpected result");
  printfn "running: %d" (Fiber.Throttle.running throttle);
  Common.test
    Dyn.unit
    (Fiber.Throttle.run throttle ~size:1 ~f:(fun () -> Fiber.return ()));
  [%expect
    {|
    caught Exit
    running: 0
    () |}]
;;
