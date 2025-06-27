open Stdune
open Fiber.O
module Scheduler = Fiber.Scheduler

let%expect_test "test fiber scheduler" =
  let ivar = Fiber.Ivar.create () in
  let f () =
    print_endline "waiting for ivar";
    let+ () = Fiber.Ivar.read ivar in
    print_endline "ivar filled"
  in
  (match Scheduler.start (Fiber.of_thunk f) with
   | Done _ -> assert false
   | Stalled s ->
     let step = Scheduler.advance s [ Fiber.Fill (ivar, ()) ] in
     (match step with
      | Done () -> ()
      | Stalled _ -> assert false));
  [%expect {|
    waiting for ivar
    ivar filled |}]
;;
