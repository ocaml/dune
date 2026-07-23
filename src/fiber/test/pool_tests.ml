open Fiber.O
open Common
module Pool = Fiber.Pool

let%expect_test "start & stop pool" =
  Scheduler.run
    (let pool = Pool.create () in
     Pool.close pool);
  [%expect {| |}]
;;

let%expect_test "run 2 tasks" =
  Scheduler.run
    (Pool.with_ (fun pool ->
       let task n () =
         printf "task %d\n" n;
         Fiber.return ()
       in
       Fiber.parallel_iter [ 1; 2 ] ~f:(fun n -> Pool.task pool ~f:(task n))));
  [%expect
    {|
    task 1
    task 2 |}]
;;

let%expect_test "raise exception" =
  Scheduler.run
    (let+ res =
       Fiber.collect_errors (fun () ->
         Pool.with_ (fun pool -> Pool.task pool ~f:(fun () -> raise Exit)))
     in
     match res with
     | Ok _ -> assert false
     | Error [ e ] ->
       assert (e.exn = Exit);
       print_endline "Caught Exit"
     | _ -> assert false);
  [%expect {| Caught Exit |}]
;;

let%expect_test "double run a pool" =
  (* Calling [Pool.run] twice on the same pool shouldn't be allowed

     We can't allow competing [Pool.run] calls to get tasks. We want to make
     sure only a single [run] will get the exceptions from all tasks in the
     pool *)
  (Scheduler.run
   @@
   let pool = Pool.create () in
   Fiber.fork_and_join_unit (fun () -> Pool.run pool) (fun () -> Pool.run pool));
  [%expect.unreachable]
[@@expect.uncaught_exn
  {| ("(\"Fiber.Pool.run: concurent calls to run aren't allowed\", {})") |}]
;;

let%expect_test "run -> stop -> run a pool" =
  (* We shouldn't be able to call [Pool.run] again after we already called
     [Pool.run] and [Pool.close]. In other words, we can't reuse pools *)
  (Scheduler.run
   @@
   let pool = Pool.create () in
   let* () =
     Fiber.fork_and_join_unit
       (fun () -> Pool.run pool)
       (fun () -> Fiber.Pool.task pool ~f:(fun () -> Pool.close pool))
   in
   Pool.run pool);
  [%expect.unreachable]
[@@expect.uncaught_exn
  {| ("(\"Fiber.Pool.run: concurent calls to run aren't allowed\", {})") |}]
;;

let%expect_test "stop a pool and then run it" =
  (Scheduler.run
   @@
   let pool = Pool.create () in
   let* () = Pool.close pool in
   Pool.run pool);
  [%expect {||}]
;;

let%expect_test "pool - weird deadlock" =
  (* this doesn't dead lock *)
  (Scheduler.run
   @@
   let pool = Pool.create () in
   let* () = Pool.task pool ~f:Fiber.return in
   Fiber.fork_and_join_unit (fun () -> Pool.close pool) (fun () -> Pool.run pool));
  [%expect {||}];
  (* but this does *)
  (Scheduler.run
   @@
   let pool = Pool.create () in
   let* () = Pool.task pool ~f:Fiber.return in
   let* () = Pool.task pool ~f:Fiber.return in
   Fiber.fork_and_join_unit (fun () -> Pool.close pool) (fun () -> Pool.run pool));
  [%expect {||}]
;;

let%expect_test "nested run in task" =
  (Scheduler.run
   @@
   let pool = Pool.create () in
   let* () = Pool.task pool ~f:(fun () -> Pool.run pool) in
   Fiber.fork_and_join_unit (fun () -> Pool.close pool) (fun () -> Pool.run pool));
  [%expect.unreachable]
[@@expect.uncaught_exn
  {| ("(\"Fiber.Pool.run: concurent calls to run aren't allowed\", {})") |}]
;;

let%expect_test "nested tasks" =
  (Scheduler.run
   @@
   let pool = Pool.create () in
   let* () =
     Pool.task pool ~f:(fun () ->
       print_endline "outer";
       let* () =
         Pool.task pool ~f:(fun () ->
           let+ () = Fiber.return () in
           print_endline "inner")
       in
       Pool.close pool)
   in
   Pool.run pool);
  [%expect
    {|
    outer
    inner |}]
;;

let%expect_test "stopping inside a task" =
  (Scheduler.run
   @@
   let pool = Pool.create () in
   let* () = Pool.task pool ~f:(fun () -> Pool.close pool) in
   Pool.run pool);
  [%expect {||}]
;;
