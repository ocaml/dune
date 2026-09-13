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

let%expect_test "at-most clauses are counted in num_clauses" =
  let open Sat in
  let p = create () in
  let a = add_variable p 1 in
  let b = add_variable p 2 in
  let c = add_variable p 3 in
  let amo = at_most_one p [ a; b ] in
  let _am2 = at_most p 2 [ a; b; c ] in
  print_stats p;
  (* Nothing is selected yet, and [a] is still undecided. *)
  print_endline (if is_some (get_selected amo) then "selected" else "unselected");
  print_endline (if is_some (get_best_undecided amo) then "undecided" else "decided");
  [%expect
    {|
    num_variables=3 num_clauses=2 num_decisions=0 num_conflicts=0
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
  let amo = at_most_one p [ a; b ] in
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
    num_variables=2 num_clauses=2 num_decisions=1 num_conflicts=0
    true
    num_variables=2 num_clauses=2 num_decisions=1 num_conflicts=0
    selected
  |}]
;;

let%expect_test "incremental deciders can detect non-chronological backtracking" =
  let open Sat in
  let p = create () in
  let a = add_variable p 1 in
  let c = add_variable p 2 in
  let d = add_variable p 3 in
  let x = add_variable p 4 in
  (* Selecting [a] and [d] implies both [x] and [not x]. The unrelated
     decision [c] makes conflict analysis backjump from level 3 to level 1. *)
  at_least_one p [ neg a; neg d; x ];
  at_least_one p [ neg a; neg d; neg x ];
  let choices = ref [ a; c; d ] in
  let rec solve level =
    match step p with
    | `Sat -> Format.printf "sat at level %d@." level
    | `Unsat -> Format.printf "unsat@."
    | `Backtrack nb_levels ->
      Format.printf "backtracked %d level(s) from level %d@." nb_levels level;
      solve (level - nb_levels)
    | `Decide ->
      Format.printf "decide at level %d@." level;
      (match !choices with
       | [] -> choose p `Any_false
       | choice :: rest ->
         choices := rest;
         choose p (`True choice));
      solve (level + 1)
  in
  solve 0;
  print_stats p;
  [%expect
    {|
    decide at level 0
    decide at level 1
    decide at level 2
    backtracked 2 level(s) from level 3
    decide at level 1
    decide at level 2
    sat at level 3
    num_variables=4 num_clauses=2 num_decisions=5 num_conflicts=1
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

let%expect_test "run_solver stops calling the decider once None" =
  let open Sat in
  let p = create () in
  let a = add_variable p 1 in
  let b = add_variable p 2 in
  let c = add_variable p 3 in
  let d = add_variable p 4 in
  at_least_one p [ a; b; c; d ];
  let choices = ref [ neg a; d ] in
  let calls = ref 0 in
  let decide () =
    incr calls;
    match !choices with
    | [] -> None
    | choice :: rest ->
      choices := rest;
      Some choice
  in
  print_endline (string_of_bool (run_solver p decide));
  Format.printf "decide calls=%d@." !calls;
  print_stats p;
  [%expect
    {|
    true
    decide calls=3
    num_variables=4 num_clauses=1 num_decisions=4 num_conflicts=0
    |}]
;;

(* Drive [step] and [choose] by hand, so that a test can add variables and
   clauses in the middle of the search. [decide] returns [`Add] once it has
   added some, to let the solver apply them before the next choice. *)
let solve p ~decide =
  let open Sat in
  let rec loop level =
    match step p with
    | `Sat -> Format.printf "sat at level %d@." level
    | `Unsat -> Format.printf "unsat@."
    | `Backtrack nb_levels ->
      Format.printf "backtracked %d level(s) to level %d@." nb_levels (level - nb_levels);
      loop (level - nb_levels)
    | `Decide ->
      (match decide level with
       | `Add -> loop level
       | (`True _ | `Any_false) as choice ->
         choose p choice;
         loop (level + 1))
  in
  loop 0
;;

let print_lit name lit = Format.printf "%s: %a@." name Pp.to_fmt (Sat.explain_reason lit)

let%expect_test "a clause added while solving outlives the decision that added it" =
  let open Sat in
  let p = create () in
  let a = add_variable p 1 in
  let c = add_variable p 3 in
  at_least_one p [ a; c ] ~reason:"a or c";
  let b = ref None in
  let script = ref [ `Select_a; `Need_b; `Reject_b ] in
  let decide _level =
    match !script with
    | [] -> `Any_false
    | action :: rest ->
      script := rest;
      (match action with
       | `Select_a -> `True a
       | `Need_b ->
         (* [b] only becomes relevant once [a] is selected. *)
         let v = add_variable p 2 in
         b := Some v;
         implies p a [ v ] ~reason:"a needs b";
         `Add
       | `Reject_b ->
         at_least_one p [ neg (Option.value_exn !b) ] ~reason:"b is impossible";
         `Add)
  in
  solve p ~decide;
  print_lit "a" a;
  print_lit "b" (Option.value_exn !b);
  print_lit "c" c;
  print_stats p;
  [%expect
    {|
    backtracked 1 level(s) to level 0
    sat at level 0
    a: 2=false => 1=false
    b: b is impossible => 2=false
    c: 1=false => 3=true
    num_variables=3 num_clauses=3 num_decisions=1 num_conflicts=0
    |}]
;;

let%expect_test "a clause added while solving can be false already" =
  let open Sat in
  let p = create () in
  let a = add_variable p 1 in
  let b = add_variable p 2 in
  let script = ref [ `Select_a; `Select_b; `Conflict ] in
  (* Something has to be left undecided after [a] and [b] are selected,
     otherwise the solver stops before asking for the next decision. *)
  let _spare = add_variable p 9 in
  let decide _level =
    match !script with
    | [] -> `Any_false
    | action :: rest ->
      script := rest;
      (match action with
       | `Select_a -> `True a
       | `Select_b -> `True b
       | `Conflict ->
         at_least_one p [ neg a; neg b ] ~reason:"a and b are exclusive";
         `Add)
  in
  solve p ~decide;
  print_lit "a" a;
  print_lit "b" b;
  print_stats p;
  [%expect
    {|
    backtracked 1 level(s) to level 1
    sat at level 2
    a: considering => 1=true
    b: 1=true => 2=false
    num_variables=3 num_clauses=1 num_decisions=3 num_conflicts=1
    |}]
;;

let%expect_test "an at-most-one clause added while solving unselects the others" =
  let open Sat in
  let p = create () in
  let a = add_variable p 1 in
  let b = add_variable p 2 in
  let c = add_variable p 3 in
  let _spare = add_variable p 9 in
  let script = ref [ `Select_a; `Exclusive; `Force_b ] in
  let decide _level =
    match !script with
    | [] -> `Any_false
    | action :: rest ->
      script := rest;
      (match action with
       | `Select_a -> `True a
       | `Exclusive ->
         let (_ : at_most_one_clause) = at_most_one p [ a; b; c ] in
         `Add
       | `Force_b ->
         (* Forces a backjump to unselect [a] *)
         at_least_one p [ b ] ~reason:"b is required";
         `Add)
  in
  solve p ~decide;
  print_lit "a" a;
  print_lit "b" b;
  print_lit "c" c;
  print_stats p;
  [%expect
    {|
    backtracked 1 level(s) to level 0
    sat at level 1
    a: 2=true => 1=false
    b: b is required => 2=true
    c: 2=true => 3=false
    num_variables=4 num_clauses=2 num_decisions=2 num_conflicts=0
    |}]
;;
