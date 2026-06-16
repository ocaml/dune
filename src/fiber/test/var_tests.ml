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

let%expect_test "Var.get_apply and get_apply_map read the var and thread the argument" =
  let var = Fiber.Var.create 0 in
  let run =
    Fiber.Var.set var 10 (fun () ->
      let* sum = Fiber.Var.get_apply var (fun value x -> Fiber.return (value + x)) 5 in
      let+ product = Fiber.Var.get_apply_map var (fun value x -> value * x) 5 in
      printf "get_apply = %d, get_apply_map = %d\n" sum product)
  in
  test unit run;
  [%expect
    {|
    get_apply = 15, get_apply_map = 50
    () |}]
;;

let%expect_test "Var.set_apply and update_apply scope the change and thread the argument" =
  let var = Fiber.Var.create 0 in
  let run =
    Fiber.Var.set var 1 (fun () ->
      let* set_inner, arg =
        Fiber.Var.set_apply
          var
          2
          (fun x ->
             let+ value = Fiber.Var.get var in
             value, x)
          100
      in
      let* set_outer = Fiber.Var.get var in
      let* update_inner =
        Fiber.Var.update_apply
          var
          ~f:(fun v -> v + 10)
          (fun x ->
             let+ value = Fiber.Var.get var in
             value + x)
          5
      in
      let+ update_outer = Fiber.Var.get var in
      printf
        "set_apply: inner=%d arg=%d outer=%d; update_apply: inner=%d outer=%d\n"
        set_inner
        arg
        set_outer
        update_inner
        update_outer)
  in
  test unit run;
  [%expect
    {|
    set_apply: inner=2 arg=100 outer=1; update_apply: inner=16 outer=1
    () |}]
;;
