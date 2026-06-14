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
