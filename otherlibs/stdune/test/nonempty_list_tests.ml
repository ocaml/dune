open Stdune

let%expect_test "collection operations" =
  let values = Nonempty_list.[ 1; 2; 3 ] in
  Printf.printf "length: %d\n" (Nonempty_list.length values);
  Nonempty_list.iter values ~f:(Printf.printf "value: %d\n");
  Printf.printf
    "all positive: %b\n"
    (Nonempty_list.for_all values ~f:(fun value -> value > 0));
  Printf.printf
    "all even: %b\n"
    (Nonempty_list.for_all values ~f:(fun value -> value mod 2 = 0));
  let sums = Nonempty_list.map2 values Nonempty_list.[ 10; 20; 30 ] ~f:( + ) in
  Nonempty_list.iter sums ~f:(Printf.printf "sum: %d\n");
  [%expect
    {|
    length: 3
    value: 1
    value: 2
    value: 3
    all positive: true
    all even: false
    sum: 11
    sum: 22
    sum: 33 |}]
;;
