open Stdune
module Al = Appendable_list

let print xs = List.iter (Al.to_list xs) ~f:print_endline

let%expect_test "empty" =
  print Al.empty;
  [%expect {| |}]
;;

let%expect_test "singleton" =
  print (Al.singleton "abc");
  [%expect {| abc |}]
;;

let%expect_test "cons" =
  print (List.fold_right [ "a"; "b"; "c"; "d" ] ~init:Al.empty ~f:Al.cons);
  [%expect
    {|
    a
    b
    c
    d |}]
;;

let%expect_test "append" =
  print
    Al.(singleton "a" @ (singleton "b" @ singleton "c") @ singleton "d" @ singleton "e");
  [%expect
    {|
    a
    b
    c
    d
    e |}];
  print Al.(cons "a" (cons "b" (cons "c" empty)) @ cons "d" (cons "e" (cons "f" empty)));
  [%expect
    {|
    a
    b
    c
    d
    e
    f |}]
;;

let%expect_test "concat" =
  print (Al.concat (List.init 10 ~f:(fun i -> Al.singleton (Int.to_string i))));
  [%expect
    {|
    0
    1
    2
    3
    4
    5
    6
    7
    8
    9 |}]
;;

let check_traversal xs expected =
  assert (Al.length xs = List.length expected);
  let remaining = ref expected in
  Al.iter xs ~f:(fun x ->
    match !remaining with
    | [] -> Code_error.raise "unexpected element" []
    | y :: rest ->
      assert (x = y);
      remaining := rest);
  assert (List.is_empty !remaining);
  assert (Array.Immutable.to_list (Al.to_immutable_array xs) = expected)
;;

let%expect_test "traverse mixed appends and concats in order" =
  let xs =
    Al.concat
      [ Al.empty
      ; Al.cons 0 (Al.of_list [ 1; 2 ])
      ; Al.(
          concat [ singleton 3; concat [ empty; of_list [ 4; 5 ]; empty ] ]
          @ concat [ singleton 6; empty; singleton 7 ])
      ; Al.concat [ Al.empty; Al.empty ]
      ; Al.singleton 8
      ; Al.empty
      ]
  in
  check_traversal xs (List.init 9 ~f:Fun.id);
  [%expect {||}]
;;

let%expect_test "traverse wide concats" =
  let expected = List.init 100 ~f:Fun.id in
  check_traversal (Al.concat (List.map expected ~f:Al.singleton)) expected;
  [%expect {||}]
;;

let%expect_test "traverse cons chains" =
  let rec make n acc = if n < 0 then acc else make (n - 1) (Al.cons n acc) in
  check_traversal (make 99 Al.empty) (List.init 100 ~f:Fun.id);
  [%expect {||}]
;;

let%expect_test "traverse left-nested appends" =
  let rec make n acc =
    if n = 100 then acc else make (n + 2) Al.(acc @ of_list [ n; n + 1 ])
  in
  check_traversal (make 0 Al.empty) (List.init 100 ~f:Fun.id);
  [%expect {||}]
;;

let%expect_test "traverse right-nested appends" =
  let rec make n acc =
    if n < 0 then acc else make (n - 2) Al.(of_list [ n; n + 1 ] @ acc)
  in
  check_traversal (make 98 Al.empty) (List.init 100 ~f:Fun.id);
  [%expect {||}]
;;

let%expect_test "traverse left-nested concats" =
  let rec make n acc =
    if n = 100 then acc else make (n + 1) (Al.concat [ acc; Al.empty; Al.singleton n ])
  in
  check_traversal (make 0 Al.empty) (List.init 100 ~f:Fun.id);
  [%expect {||}]
;;

let%expect_test "traverse right-nested concats" =
  let rec make n acc =
    if n < 0 then acc else make (n - 1) (Al.concat [ Al.singleton n; Al.empty; acc ])
  in
  check_traversal (make 99 Al.empty) (List.init 100 ~f:Fun.id);
  [%expect {||}]
;;

let%expect_test "is_empty" =
  let assert_empty l = assert (Al.is_empty l) in
  assert_empty Al.empty;
  [%expect {||}];
  assert_empty @@ Al.concat [];
  [%expect {||}];
  assert_empty @@ Al.concat [ Al.empty ];
  [%expect {||}];
  assert_empty @@ Al.concat [ Al.empty; Al.empty ];
  [%expect {||}];
  assert_empty @@ Al.of_list [];
  [%expect {||}]
;;
