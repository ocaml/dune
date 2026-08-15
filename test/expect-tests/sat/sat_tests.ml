open Stdune
open Dune_tests_common

let () = init ()

module Sat = Sat.Make (struct
    type t = int

    let pp = Pp.textf "%d"
  end)

let print_stats p =
  let open Sat in
  let st = get_stats p in
  Format.printf
    "num_variables=%d num_clauses=%d num_decisions=%d num_conflicts=%d@."
    st.num_variables
    st.num_clauses
    st.num_decisions
    st.num_conflicts
;;

let is_some = function
  | Some _ -> true
  | None -> false
;;

let%expect_test "a fresh problem has all-zero stats" =
  let open Sat in
  let p = create () in
  print_stats p;
  [%expect
    {|
    num_variables=0 num_clauses=0 num_decisions=0 num_conflicts=0
  |}]
;;

let%expect_test "structural counters track added variables and input clauses" =
  let open Sat in
  let p = create () in
  let a = add_variable p 1 in
  let b = add_variable p 2 in
  let c = add_variable p 3 in
  print_stats p;
  at_least_one p [ a; b ];
  implies p b [ c ];
  print_stats p;
  impossible p;
  print_stats p;
  [%expect
    {|
    num_variables=3 num_clauses=0 num_decisions=0 num_conflicts=0
    num_variables=3 num_clauses=2 num_decisions=0 num_conflicts=0
    num_variables=3 num_clauses=3 num_decisions=0 num_conflicts=0
  |}]
;;

let%expect_test "at-most clauses are not counted in num_clauses" =
  let open Sat in
  let p = create () in
  let a = add_variable p 1 in
  let b = add_variable p 2 in
  let c = add_variable p 3 in
  let amo = at_most_one [ a; b ] in
  let _am2 = at_most 2 [ a; b; c ] in
  print_stats p;
  (* The at-most clauses are real even though they are not counted:
     nothing is selected yet, and [a] is still undecided. *)
  print_endline (if is_some (get_selected amo) then "selected" else "unselected");
  print_endline (if is_some (get_best_undecided amo) then "undecided" else "decided");
  [%expect
    {|
    num_variables=3 num_clauses=0 num_decisions=0 num_conflicts=0
    unselected
    undecided
  |}]
;;

let%expect_test "run_solver records decisions, and counters are cumulative" =
  let open Sat in
  let p = create () in
  let a = add_variable p 1 in
  let b = add_variable p 2 in
  at_least_one p [ a; b ];
  let amo = at_most_one [ a; b ] in
  let decide () = get_best_undecided amo in
  print_endline (string_of_bool (run_solver p decide));
  print_stats p;
  (* Re-solving the same (already solved) problem is a no-op: all
     variables are decided, so no new decisions or conflicts happen. *)
  print_endline (string_of_bool (run_solver p decide));
  print_stats p;
  print_endline (if is_some (get_selected amo) then "selected" else "unselected");
  [%expect
    {|
    true
    num_variables=2 num_clauses=1 num_decisions=1 num_conflicts=0
    true
    num_variables=2 num_clauses=1 num_decisions=1 num_conflicts=0
    selected
  |}]
;;

let%expect_test "impossible problems are rejected before solving" =
  let open Sat in
  let p = create () in
  impossible p;
  print_stats p;
  print_endline (string_of_bool (run_solver p (fun () -> None)));
  print_stats p;
  [%expect
    {|
    num_variables=0 num_clauses=1 num_decisions=0 num_conflicts=0
    false
    num_variables=0 num_clauses=1 num_decisions=0 num_conflicts=0
  |}]
;;

let%expect_test "run_solver records conflicts on an unsatisfiable problem" =
  let open Sat in
  let p = create () in
  let a = add_variable p 1 in
  let b = add_variable p 2 in
  (* (a or b) and (!a or b) and (a or !b) and (!a or !b) is unsatisfiable. *)
  at_least_one p [ a; b ];
  at_least_one p [ neg a; b ];
  at_least_one p [ a; neg b ];
  at_least_one p [ neg a; neg b ];
  (* Returning [None] from the decider means: set the remaining variables
     to [False]. The search then discovers both conflicts. *)
  print_endline (string_of_bool (run_solver p (fun () -> None)));
  print_stats p;
  [%expect
    {|
    false
    num_variables=2 num_clauses=4 num_decisions=1 num_conflicts=2
  |}]
;;
