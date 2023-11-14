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
  [%expect {|
    a
    b
    c
    d |}]
;;

let%expect_test "append" =
  print
    Al.(singleton "a" @ (singleton "b" @ singleton "c") @ singleton "d" @ singleton "e");
  [%expect {|
    a
    b
    c
    d
    e |}];
  print Al.(cons "a" (cons "b" (cons "c" empty)) @ cons "d" (cons "e" (cons "f" empty)));
  [%expect {|
    a
    b
    c
    d
    e
    f |}]
;;

let%expect_test "concat" =
  print (Al.concat (List.init 10 ~f:(fun i -> Al.singleton (Int.to_string i))));
  [%expect {|
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
