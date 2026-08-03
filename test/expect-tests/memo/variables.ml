(* Tests for Memo variables. *)

open! Stdune
open! Memo.O
open Test_helpers.Make ()

let%expect_test "create and read" =
  let var = Memo.Var.create ~name:"foo" 200 in
  run (Memo.Var.read var) |> printfn "var: %d";
  [%expect {| var: 200 |}]
;;

let%expect_test "invalidation" =
  let var = Memo.Var.create ~name:"foo" 200 in
  let run () = run (Memo.Var.read var) |> printfn "var: %d" in
  run ();
  [%expect {| var: 200 |}];
  let invalidation = Memo.Var.set var 400 in
  Memo.Invalidation.details_hum invalidation |> String.concat ~sep:"\n" |> print_endline;
  Memo.reset invalidation;
  run ();
  [%expect
    {|
    Variable foo changed
    var: 400
    |}]
;;

let%expect_test "cutoff" =
  let var = Memo.Var.create ~name:"foo" ~cutoff:(fun x y -> x mod 2 = y mod 2) 200 in
  let node = Memo.Var.read var |> Memo.map ~f:Fun.id in
  let set_invalidate_print_run value =
    let invalidation = Memo.Var.set var value in
    Memo.reset invalidation;
    node |> run |> printfn "var: %d"
  in
  set_invalidate_print_run 202;
  [%expect {| var: 200 |}];
  set_invalidate_print_run 203;
  [%expect {| var: 203 |}];
  set_invalidate_print_run 202;
  [%expect {| var: 202 |}]
;;

let%expect_test "unit variable" =
  let var = Memo.Var.Unit.create () in
  let runs = ref 0 in
  let node =
    Memo.lazy_ ~name:"unit-var-dependent" (fun () ->
      let+ () = Memo.Var.Unit.read var in
      incr runs;
      !runs)
  in
  let print_state () = printfn "runs: %d" (run (Memo.Lazy.force node)) in
  print_state ();
  print_state ();
  [%expect
    {|
    runs: 1
    runs: 1
    |}];
  let invalidation = Memo.Var.Unit.invalidate var ~reason:Memo.Invalidation.Reason.Test in
  Memo.Invalidation.details_hum invalidation |> String.concat ~sep:"\n" |> print_endline;
  Memo.reset invalidation;
  print_state ();
  print_state ();
  [%expect
    {|
    Rebuild initiated by an internal testsuite
    runs: 2
    runs: 2
    |}]
;;
