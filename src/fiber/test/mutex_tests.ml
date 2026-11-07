open Stdune
open Fiber.O
open Common

let%expect_test "Mutex serializes fibers and releases after errors" =
  let mutex = Fiber.Mutex.create () in
  let inside = ref false in
  let max_inside = ref 0 in
  let inside_count = ref 0 in
  let critical_section () =
    Fiber.Mutex.with_lock mutex ~f:(fun () ->
      if !inside then print_endline "overlap";
      inside := true;
      incr inside_count;
      max_inside := max !max_inside !inside_count;
      let* () = Scheduler.yield () in
      decr inside_count;
      inside := false;
      Fiber.return ())
  in
  Scheduler.run (Fiber.parallel_iter [ (); (); () ] ~f:critical_section);
  printfn "max inside: %d" !max_inside;
  let result =
    Scheduler.run
      (Fiber.collect_errors (fun () ->
         Fiber.Mutex.with_lock mutex ~f:(fun () -> raise Exit)))
  in
  (match result with
   | Error [ { exn = Exit; _ } ] -> print_endline "caught Exit"
   | Ok () | Error _ -> print_endline "unexpected result");
  Scheduler.run
    (Fiber.Mutex.with_lock mutex ~f:(fun () ->
       print_endline "reacquired";
       Fiber.return ()));
  [%expect
    {|
    max inside: 1
    caught Exit
    reacquired |}]
;;
