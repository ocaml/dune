open Stdune
open Dune_tests_common

let () = init ()

type mynode = { name : string }

let pp_mynode fmt n = Format.fprintf fmt "%s" n.name

let%expect_test _ =
  let open
    Dag.Make
      (struct
        type t = mynode
      end)
      () in
  let node data = create_node data in
  let root = node { name = "root" } in
  let node11 = node { name = "child 1 1" } in
  let node12 = node { name = "child 1 2" } in
  let node21 = node { name = "child 2 1" } in
  let node31 = node { name = "child 3 1" } in
  add_assuming_missing root node11;
  add_assuming_missing root node12;
  add_assuming_missing node12 node21;
  add_assuming_missing node21 node31;
  let dag_pp_mynode = pp_node pp_mynode in
  Format.printf "%a@." dag_pp_mynode root;
  let node41 = node { name = "child 4 1" } in
  add_assuming_missing node31 node41;
  Format.printf "%a@." dag_pp_mynode root;
  let name node = (value node).name in
  try
    add_assuming_missing node41 root;
    print_endline "no cycle"
  with
  | Cycle cycle ->
    print_endline "cycle:";
    let cycle = List.map cycle ~f:name in
    List.map ~f:Pp.text cycle |> Pp.concat ~sep:Pp.space |> print;
    [%expect
      {|
      (0: k=1) (root) [(2: k=1) (child 1 2) [(3: k=1) (child 2 1) [(4: k=2) (child 3 1) [
                                                                   ]]];
                        (1: k=1) (child 1 1) []]
      (0: k=1) (root) [(2: k=1) (child 1 2) [(3: k=1) (child 2 1) [(4: k=2) (child 3 1) [
                                                                   (5: k=2) (child 4 1) [
                                                                   ]]]];
                        (1: k=1) (child 1 1) []]
      cycle:
      child 4 1 child 3 1 child 2 1 child 1 2
      root
    |}]
;;

let rec adjacent_pairs l =
  match l with
  | [] | [ _ ] -> []
  | x :: y :: rest -> (x, y) :: adjacent_pairs (y :: rest)
;;

let cycle_test variant =
  let open
    Dag.Make
      (struct
        type t = int
      end)
      () in
  let node data = create_node data in
  let edges = ref [] in
  let add n1 n2 =
    edges := (value n1, value n2) :: !edges;
    add_assuming_missing n1 n2
  in
  let _n1 = node 1 in
  let n2 = node 2 in
  let n3 = node 3 in
  (* the two variants are equivalent, but they end up taking a different code
     path when producing the cycle for some reason (or at least they did in
     2019-03) *)
  (match variant with
   | `a -> add n2 n3
   | `b -> ());
  let n4 = node 4 in
  add n3 n4;
  let n5 = node 5 in
  add n5 n2;
  let n6 = node 6 in
  add n6 n3;
  let n7 = node 7 in
  let n8 = node 8 in
  add n7 n8;
  let n9 = node 9 in
  add n8 n9;
  let n10 = node 10 in
  add n9 n10;
  let n11 = node 11 in
  add n10 n11;
  let n12 = node 12 in
  add n11 n12;
  let n13 = node 13 in
  add n12 n13;
  let n14 = node 14 in
  add n13 n14;
  let n15 = node 15 in
  add n14 n15;
  let n16 = node 16 in
  add n15 n16;
  let n17 = node 17 in
  add n16 n17;
  let n18 = node 18 in
  add n17 n18;
  let n19 = node 19 in
  add n12 n19;
  let n20 = node 20 in
  add n10 n20;
  let n21 = node 21 in
  add n20 n21;
  let n22 = node 22 in
  add n21 n22;
  let n23 = node 23 in
  add n22 n23;
  let n24 = node 24 in
  add n23 n24;
  let n25 = node 25 in
  add n24 n25;
  let n26 = node 26 in
  add n25 n26;
  let n27 = node 27 in
  add n26 n27;
  let n28 = node 28 in
  add n21 n28;
  let n29 = node 29 in
  add n10 n29;
  let n30 = node 30 in
  add n8 n30;
  let _n31 = node 31 in
  add n14 n20;
  match add n23 n11 with
  | _ -> assert false
  | exception Cycle c ->
    let c = List.map c ~f:value in
    List.iter (adjacent_pairs c) ~f:(fun (b, a) ->
      match List.exists !edges ~f:(fun edge -> edge = (a, b)) with
      | true -> ()
      | false -> Printf.ksprintf failwith "bad edge in cycle: (%d, %d)\n" a b);
    List.map c ~f:(Pp.textf "%d") |> Pp.concat ~sep:Pp.space |> print
;;

let%expect_test _ =
  cycle_test `a;
  [%expect {|
    23 22 21 20 14 13 12
    11
  |}]
;;

let%expect_test _ =
  cycle_test `b;
  [%expect {|
    23 22 21 20 14 13 12
    11
  |}]
;;

let%expect_test "creating a cycle can succeed on the second attempt" =
  let open
    Dag.Make
      (struct
        type t = mynode
      end)
      () in
  let node = create_node in
  let c1 = node { name = "c1" } in
  let c2 = node { name = "c2" } in
  let c3 = node { name = "c3" } in
  let c4 = node { name = "c4" } in
  add_assuming_missing c1 c2;
  add_assuming_missing c2 c3;
  add_assuming_missing c3 c4;
  let dag_pp_mynode = pp_node pp_mynode in
  Format.printf "c1 = %a@.\n" dag_pp_mynode c1;
  Format.printf "c2 = %a@.\n" dag_pp_mynode c2;
  Format.printf "c3 = %a@.\n" dag_pp_mynode c3;
  Format.printf "c4 = %a@.\n" dag_pp_mynode c4;
  [%expect
    {|
    c1 = (0: k=1) (c1) [(1: k=1) (c2) [(2: k=1) (c3) [(3: k=2) (c4) []]]]

    c2 = (1: k=1) (c2) [(2: k=1) (c3) [(3: k=2) (c4) []]]

    c3 = (2: k=1) (c3) [(3: k=2) (c4) []]

    c4 = (3: k=2) (c4) []
  |}];
  (match add_assuming_missing c4 c2 with
   | () -> Format.printf "added :o\n"
   | exception Cycle _ -> Format.printf "cycle\n");
  Format.printf "c1 = %a@.\n" dag_pp_mynode c1;
  Format.printf "c2 = %a@.\n" dag_pp_mynode c2;
  Format.printf "c3 = %a@.\n" dag_pp_mynode c3;
  Format.printf "c4 = %a@.\n" dag_pp_mynode c4;
  (* Note that the state of the nodes changed even though adding the edge has
     failed. Specifically, the levels of nodes c2 and c3 increased to 2. *)
  [%expect
    {|
    cycle
    c1 = (0: k=1) (c1) [(1: k=2) (c2) [(2: k=2) (c3) [(3: k=2) (c4) []]]]

    c2 = (1: k=2) (c2) [(2: k=2) (c3) [(3: k=2) (c4) []]]

    c3 = (2: k=2) (c3) [(3: k=2) (c4) []]

    c4 = (3: k=2) (c4) []
  |}];
  (match add_assuming_missing c4 c2 with
   | () -> Format.printf "added :o\n"
   | exception Cycle _ -> Format.printf "cycle\n");
  Format.printf "c1 = %a@.\n" dag_pp_mynode c1;
  (* The output is truncated at depth 20. *)
  [%expect
    {|
    added :o
    c1 = (0: k=1) (c1) [(1: k=2) (c2) [(2: k=2) (c3) [(3: k=2) (c4) [
                                                               (1: k=2) (c2) [
                                                               (2: k=2) (c3) [
                                                               (3: k=2) (c4) [
                                                               (1: k=2) (c2) [
                                                               (2: k=2) (c3) [
                                                               (3: k=2) (c4) [
                                                               (1: k=2) (c2) [
                                                               (2: k=2) (c3) [
                                                               (3: k=2) (c4) [
                                                               (1: k=2) (c2) [
                                                               (2: k=2) (c3) [
                                                               (3: k=2) (c4) [
                                                               (1: k=2) (c2) [
                                                               (2: k=2) (c3) [
                                                               (3: k=2) (c4) [
                                                               (1: k=2) (c2) [
                                                               ...]]]]]]]]]]]]]]]]]]]]
  |}]
;;
