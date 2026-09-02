(** Generate random SAT problems with `at_least_one` and `at_most` constraints,
    then check that a naive bruteforce search produces the exact same first
    solution (or unsat) as the SAT solver on those problems. The boolean
    choices for the variable assignments are made in the same order for both
    searches (otherwise their solutions could differ).

    This test is intended to detect any wrong optimization performed by the
    smart SAT solver, since a divergence from the bruteforce solver would break
    the assumptions of the [opam_solver]. *)

open Stdune
open Dune_tests_common

let () = init ()

module Sat = Sat.Make (struct
    type t = int

    let pp = Pp.textf "%d"
  end)

type constraint_ =
  | At_least_one of int list
  | At_most of int * int list

type t =
  { rng : Random.State.t
  ; problem : Sat.t
  ; mutable lits : Sat.lit Int.Map.t
  ; mutable preferred : bool Int.Map.t
  ; mutable order : int list
  ; mutable nb_vars : int
  ; mutable constraints : constraint_ list
  }

let create ~seed ~iteration =
  { rng = Random.State.make [| seed; iteration |]
  ; problem = Sat.create ()
  ; lits = Int.Map.empty
  ; preferred = Int.Map.empty
  ; order = []
  ; nb_vars = 0
  ; constraints = []
  }
;;

let add_variable t =
  let i = t.nb_vars + 1 in
  t.nb_vars <- i;
  t.lits <- Int.Map.set t.lits i (Sat.add_variable t.problem i);
  t.preferred <- Int.Map.set t.preferred i (Random.State.bool t.rng);
  t.order <- t.order @ [ i ];
  i
;;

let shuffle_list ~rng lst =
  let arr = Array.of_list lst in
  for i = Array.length arr - 1 downto 1 do
    let j = Random.State.int rng (i + 1) in
    let x = arr.(i) in
    arr.(i) <- arr.(j);
    arr.(j) <- x
  done;
  Array.to_list arr
;;

let lit t i =
  let lit = Int.Map.find_exn t.lits (abs i) in
  if i > 0 then lit else Sat.neg lit
;;

let preferred_lit t i = lit t (if Int.Map.find_exn t.preferred i then i else -i)
let maybe_neg ~rng i = if Random.State.bool rng then i else -i

let random_literals t ~max_size =
  let rec pick acc n =
    if n = 0
    then acc
    else (
      let i = 1 + Random.State.int t.rng t.nb_vars in
      if List.exists acc ~f:(fun j -> abs j = i)
      then pick acc (n - 1)
      else pick (maybe_neg ~rng:t.rng i :: acc) (n - 1))
  in
  pick [] (1 + Random.State.int t.rng max_size)
;;

let add_constraint t =
  match random_literals t ~max_size:4 with
  | [] -> ()
  | _ :: _ as vars ->
    let lits = List.map vars ~f:(lit t) in
    if Random.State.int t.rng 4 = 0
    then (
      let nb = Random.State.int t.rng (List.length vars) in
      t.constraints <- At_most (nb, vars) :: t.constraints;
      let (_ : Sat.at_most_clause) = Sat.at_most t.problem nb lits in
      ())
    else (
      t.constraints <- At_least_one vars :: t.constraints;
      Sat.at_least_one t.problem lits)
;;

let value t i =
  match Sat.lit_value (lit t i) with
  | True -> Some true
  | False -> Some false
  | Undecided -> None
;;

let value_exn t i =
  match value t i with
  | Some value -> value
  | None -> failwith "fuzz: the solution left a variable undecided"
;;

let decide t () =
  let rec pick = function
    | [] -> None
    | i :: rest ->
      (match value t i with
       | Some (_ : bool) -> pick rest
       | None -> Some (preferred_lit t i))
  in
  pick t.order
;;

let solve t =
  let budget = ref 10_000 in
  let rec loop () =
    decr budget;
    if !budget <= 0 then failwith "fuzz: the solver did not terminate";
    match Sat.step t.problem with
    | `Sat -> true
    | `Unsat -> false
    | `Backtrack _ -> loop ()
    | `Decide ->
      if Random.State.int t.rng 3 = 0
      then (
        if Random.State.bool t.rng then ignore (add_variable t : int);
        add_constraint t)
      else (
        match decide t () with
        | Some lit -> Sat.choose t.problem (`True lit)
        | None -> failwith "fuzz: [step] asked for a choice with nothing undecided");
      loop ()
  in
  loop ()
;;

let solution t = Int.Map.of_list_exn (List.map t.order ~f:(fun i -> i, value_exn t i))

let holds constraint_ ~value_of_var =
  let is_true i = if i > 0 then value_of_var i else not (value_of_var (-i)) in
  match constraint_ with
  | At_least_one vars -> List.exists vars ~f:is_true
  | At_most (nb, vars) -> List.length (List.filter vars ~f:is_true) <= nb
;;

let brute_force t =
  let rec search assignment = function
    | [] ->
      let value_of_var = Int.Map.find_exn assignment in
      if List.for_all t.constraints ~f:(holds ~value_of_var)
      then Some assignment
      else None
    | i :: rest ->
      let try_ value = search (Int.Map.set assignment i value) rest in
      let preferred = Int.Map.find_exn t.preferred i in
      (match try_ preferred with
       | Some _ as solution -> solution
       | None -> try_ (not preferred))
  in
  search Int.Map.empty t.order
;;

let string_of_constraint =
  let string_of_vars vars = String.concat ~sep:" " (List.map vars ~f:string_of_int) in
  function
  | At_least_one vars -> Printf.sprintf "at_least_one [%s]" (string_of_vars vars)
  | At_most (nb, vars) -> Printf.sprintf "at_most %d [%s]" nb (string_of_vars vars)
;;

let string_of_solution assignment =
  String.concat
    ~sep:" "
    (List.map (Int.Map.to_list assignment) ~f:(fun (i, value) ->
       Printf.sprintf "%d=%b" i value))
;;

let run ~seed ~nb_iterations =
  let nb_failures = ref 0 in
  let fail t iteration message =
    incr nb_failures;
    Format.printf "iteration %d: %s@." iteration message;
    Format.printf "  %d variables@." t.nb_vars;
    Format.printf
      "  decided in the order %s@."
      (String.concat ~sep:" " (List.map t.order ~f:string_of_int));
    Format.printf "  preferring %s@." (string_of_solution t.preferred);
    List.iter (List.rev t.constraints) ~f:(fun constraint_ ->
      Format.printf "  %s@." (string_of_constraint constraint_))
  in
  for iteration = 1 to nb_iterations do
    let t = create ~seed ~iteration in
    for _ = 1 to 6 + Random.State.int t.rng 6 do
      ignore (add_variable t : int)
    done;
    t.order <- shuffle_list ~rng:t.rng t.order;
    for _ = 1 to 6 + Random.State.int t.rng 8 do
      add_constraint t
    done;
    match solve t, brute_force t with
    | false, None -> ()
    | true, Some expected ->
      let actual = solution t in
      if not (Int.Map.equal actual expected ~equal:Bool.equal)
      then (
        fail t iteration "the solution is different";
        Format.printf "  brute force: %s@." (string_of_solution expected);
        Format.printf "  solver:      %s@." (string_of_solution actual))
    | true, None -> fail t iteration "reported a solution, but there is none"
    | false, Some _ -> fail t iteration "reported unsat, but a solution exists"
  done;
  Format.printf "%d failure(s) in %d iterations@." !nb_failures nb_iterations
;;

let%expect_test "SAT solver reaches the same solution as a brute-force search" =
  run ~seed:42 ~nb_iterations:1000;
  [%expect {| 0 failure(s) in 1000 iterations |}]
;;
