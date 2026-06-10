(* Tests for dependency-cycle detection and recovery across runs. *)

open! Stdune
open! Memo.O
open Test_helpers.Make ()

(* While [cyclic] is set, [node i] depends on itself, forming a direct dependency
   cycle during its own computation. Once the cycle is removed the node must be
   recomputed - not served the cached cycle error - which exercises the rule that
   dependency-cycle errors are non-reproducible. *)
let%expect_test "a self-dependency cycle is detected, then recomputed after removal" =
  let cyclic = Memo.Var.create ~name:"cyclic" true in
  let node = ref None in
  let table =
    Memo.create
      "node"
      ~input:(module Int)
      (fun i ->
         let* cyclic = Memo.Var.read cyclic in
         if cyclic then Memo.exec (Option.value_exn !node) i else Memo.return (i + 100))
  in
  node := Some table;
  evaluate_and_print table 0;
  [%expect
    {|
    Dependency cycle detected:
    - ("node", 0)
    f 0 = Error [ { exn = "Cycle_error.E [ (\"node\", 0) ]"; backtrace = "" } ]
    |}];
  Memo.reset (Memo.Var.set cyclic false);
  evaluate_and_print table 0;
  [%expect {| f 0 = Ok 100 |}];
  Memo.reset Memo.Invalidation.empty
;;
