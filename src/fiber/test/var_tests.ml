open Fiber.O
open Dyn
open Common

let%expect_test "fiber vars are preserved across yields" =
  let var = Fiber.Var.create None in
  let fiber th () =
    let* v = Fiber.Var.get var in
    assert (v = None);
    Fiber.Var.set var (Some th) (fun () ->
      let* v = Fiber.Var.get var in
      assert (v = Some th);
      let* () = Scheduler.yield () in
      let+ v = Fiber.Var.get var in
      assert (v = Some th))
  in
  let run = Fiber.fork_and_join_unit (fiber 1) (fiber 2) in
  test unit run;
  [%expect
    {|
    () |}]
;;
