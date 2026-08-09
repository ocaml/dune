open Stdune
open Fiber.O
open Common

let%expect_test "svar" =
  let module Svar = Fiber.Svar in
  let run () =
    let svar = Svar.create 10 in
    printfn "read: %d" (Svar.read svar);
    let* () = Svar.write svar (Svar.read svar + 1) in
    let* () = Svar.write svar (Svar.read svar + 1) in
    printfn "read: %d" (Svar.read svar);
    Fiber.fork_and_join_unit
      (fun () ->
         printfn "waiter: waiting for value > 15";
         let+ () = Svar.wait svar ~until:(fun x -> x > 15) in
         printfn "wait: %d" (Svar.read svar))
      (fun () ->
         printfn "setter: modifying value to 17";
         Svar.write svar 17)
  in
  Scheduler.run (Fiber.of_thunk run);
  [%expect
    {|
    read: 10
    read: 12
    waiter: waiting for value > 15
    setter: modifying value to 17
    wait: 17 |}]
;;

let%expect_test "Svar.write wakes all matching waiters in their contexts" =
  let module Svar = Fiber.Svar in
  let svar = Svar.create 0 in
  let var = Fiber.Var.create 0 in
  let ready1 = Fiber.Ivar.create () in
  let ready2 = Fiber.Ivar.create () in
  let ready3 = Fiber.Ivar.create () in
  let awakened = ref 0 in
  let correct_context = ref 0 in
  let waiters () =
    Fiber.parallel_iter
      [ 1, 1, ready1; 2, 1, ready2; 3, 2, ready3 ]
      ~f:(fun (id, expected, ready) ->
        Fiber.Var.set var id (fun () ->
          let* () = Fiber.Ivar.fill ready () in
          let* () = Svar.wait svar ~until:(( = ) expected) in
          let+ value = Fiber.Var.get var in
          incr awakened;
          if value = id then incr correct_context))
  in
  let writer () =
    let* () = Fiber.Ivar.read ready1 in
    let* () = Fiber.Ivar.read ready2 in
    let* () = Fiber.Ivar.read ready3 in
    let* () = Svar.write svar 1 in
    let* () = Scheduler.yield () in
    printfn "after 1: awakened=%d contexts=%d" !awakened !correct_context;
    let* () = Svar.write svar 2 in
    let* () = Scheduler.yield () in
    printfn "after 2: awakened=%d contexts=%d" !awakened !correct_context;
    Fiber.return ()
  in
  Scheduler.run (Fiber.fork_and_join_unit waiters writer);
  [%expect
    {|
    after 1: awakened=2 contexts=2
    after 2: awakened=3 contexts=3 |}]
;;
