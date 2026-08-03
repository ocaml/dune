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

(* A cycle spanning several nodes: while the graph has 0 -> 1 -> 2 -> 0, node 0
   depends on itself transitively. Once the edge is broken, the formerly-cyclic
   node must recompute to a value rather than stay the cached cycle error. *)
let%expect_test "a multi-node dependency cycle is detected, then recovered after removal" =
  let graph : [ `Goto of int | `Stop of int ] array =
    Array.init 4 ~f:(function
      | 0 -> `Goto 1
      | 1 -> `Goto 2
      | 2 -> `Goto 0
      | _ -> `Stop 42)
  in
  let gate = Memo.Var.create ~name:"gate" () in
  let table =
    Memo.create_rec
      "node"
      ~input:(module Int)
      (fun f i ->
         let* () = Memo.Var.read gate in
         match graph.(i) with
         | `Goto j -> f j
         | `Stop result -> Memo.return result)
  in
  evaluate_and_print table 0;
  [%expect
    {|
    Dependency cycle detected:
    - ("node", 2)
    - called by ("node", 1)
    - called by ("node", 0)
    f 0 = Error
            [ { exn =
                  "Cycle_error.E [ (\"node\", 2); (\"node\", 1); (\"node\", 0) ]"
              ; backtrace = ""
              }
            ]
    |}];
  (* Break the cycle and advance the run so the cached cycle error is discarded. *)
  graph.(0) <- `Stop 42;
  Memo.reset (Memo.Var.set gate ());
  evaluate_and_print table 0;
  [%expect {| f 0 = Ok 42 |}];
  Memo.reset Memo.Invalidation.empty
;;
