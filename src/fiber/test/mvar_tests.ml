open Fiber.O
open Dyn
open Common
module Mvar = Fiber.Mvar

let%expect_test "created mvar is empty" =
  test
    ~expect_never:true
    opaque
    (let mvar : int Mvar.t = Mvar.create () in
     Mvar.read mvar);
  [%expect
    {|
    [PASS] Never raised as expected |}]
;;

let%expect_test "reading from written mvar consumes value" =
  test
    unit
    (let mvar = Mvar.create () in
     let value = "foo" in
     let* () = Mvar.write mvar value in
     let+ x = Mvar.read mvar in
     assert (value = x);
     print_endline "[PASS] mvar contains expected value");
  [%expect
    {|
    [PASS] mvar contains expected value
    () |}]
;;

let%expect_test "reading from empty mvar blocks" =
  test
    unit
    (let mvar = Mvar.create () in
     let value = "foo" in
     Fiber.fork_and_join_unit
       (fun () ->
          print_endline "reading mvar";
          let+ x = Mvar.read mvar in
          assert (value = x);
          print_endline "[PASS] mvar contains expected value")
       (fun () ->
          let* () = long_running_fiber () in
          print_endline "writing mvar";
          let+ () = Mvar.write mvar value in
          print_endline "written mvar"));
  [%expect
    {|
    reading mvar
    writing mvar
    written mvar
    [PASS] mvar contains expected value
    () |}]
;;

let%expect_test "writing multiple values" =
  test
    unit
    (let mvar = Mvar.create () in
     let write (n : int) : unit Fiber.t =
       Printf.printf "writing %d\n" n;
       Mvar.write mvar n
     in
     let read () =
       let+ n = Mvar.read mvar in
       Printf.printf "read %d\n" n;
       n
     in
     let rec produce n =
       let* () = write n in
       if n = 0 then Fiber.return () else produce (n - 1)
     in
     let rec consume () =
       let* n = read () in
       if n = 0 then Fiber.return () else consume ()
     in
     Fiber.fork_and_join_unit (fun () -> produce 3) consume);
  (* Writing to a mvar only blocks if the mvar is full. Similarly, reading from
     a mvar only blocks if the mvar is empty. This is why [write] and [read]
     operations in the output below are grouped two by two. *)
  [%expect
    {|
    writing 3
    writing 2
    read 3
    read 2
    writing 1
    writing 0
    read 1
    read 0
    () |}]
;;

let%expect_test "writing multiple values" =
  test
    unit
    (let m = Mvar.create () in
     Fiber.fork_and_join_unit
       (fun () ->
          print_endline "reader1: reading";
          let* x = Mvar.read m in
          printf "reader1: got %d\n" x;
          print_endline "reader1: writing";
          Mvar.write m 1)
       (fun () ->
          let* () = Scheduler.yield () in
          print_endline "reader2: writing";
          let* () = Mvar.write m 2 in
          print_endline "reader2: reading";
          let+ x = Mvar.read m in
          printf "reader2: got %d\n" x));
  [%expect
    {|
    reader1: reading
    reader2: writing
    reader2: reading
    reader1: got 2
    reader1: writing
    reader2: got 1
    () |}]
;;
