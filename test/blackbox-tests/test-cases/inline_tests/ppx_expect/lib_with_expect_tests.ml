
let%expect_test "addition" =
  Printf.printf "%d" (1 + 2);
  [%expect {| 4 |}]
