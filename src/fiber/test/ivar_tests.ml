open Fiber.O
open Dyn
open Common

let%expect_test "execution context of ivars" =
  (* The point of this test it show that the execution context is restored when
     a fiber that's blocked on an ivar is resumed. This means that fiber local
     variables are visible for example*)
  let open Fiber.O in
  let ivar = Fiber.Ivar.create () in
  let run_when_filled () =
    let var = Fiber.Var.create None in
    Fiber.Var.set var (Some 42) (fun () ->
      let peek = Fiber.Ivar.peek ivar in
      assert (peek = None);
      let* () = Fiber.Ivar.read ivar in
      let+ value = Fiber.Var.get_exn var in
      Printf.printf "var value %d\n" value)
  in
  let run = Fiber.fork_and_join_unit run_when_filled (Fiber.Ivar.fill ivar) in
  test unit run;
  [%expect
    {|
    var value 42
    () |}]
;;

let%expect_test "fill returns a fiber that executes before waiters are awoken" =
  let ivar = Fiber.Ivar.create () in
  let open Fiber.O in
  let waiters () =
    let waiter n () =
      let+ () = Fiber.Ivar.read ivar in
      Printf.printf "waiter %d resumed\n" n
    in
    Fiber.fork_and_join_unit (waiter 1) (waiter 2)
  in
  let run () =
    let* () = Scheduler.yield () in
    let+ () = Fiber.Ivar.fill ivar () in
    Printf.printf "ivar filled\n"
  in
  test unit (Fiber.fork_and_join_unit waiters run);
  [%expect
    {|
    ivar filled
    waiter 1 resumed
    waiter 2 resumed
    () |}]
;;

let%expect_test "stack usage with consecutive Ivar.fill" =
  let stack_size () = (Gc.stat ()).stack_size in
  let rec loop acc prev n =
    if n = 0
    then acc, prev
    else (
      let next = Fiber.Ivar.create () in
      let fiber =
        let* () = Fiber.Ivar.read prev in
        Fiber.Ivar.fill next ()
      in
      loop (fiber :: acc) next (n - 1))
  in
  let stack_usage n =
    let first = Fiber.Ivar.create () in
    let fibers, final = loop [] first n in
    let* () = Fiber.parallel_iter fibers ~f:Fun.id
    and* n =
      let init = stack_size () in
      let+ () = Fiber.Ivar.read final in
      stack_size () - init
    and* () = Fiber.Ivar.fill first () in
    Fiber.return n
  in
  let n0 = Scheduler.run (stack_usage 0) in
  let n1000 = Scheduler.run (stack_usage 1000) in
  if n0 = n1000
  then printf "[PASS]"
  else
    printf
      "[FAIL]\nStack usage for n = 0:    %d words\nStack usage for n = 1000: %d words\n"
      n0
      n1000;
  [%expect
    {|
    [PASS] |}]
;;
