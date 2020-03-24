open! Stdune
module Glob = Dune_glob.V1

let printf = Printf.printf

let test glob s =
  printf "%S matches %S == %b" (Glob.to_string glob) s (Glob.test glob s)

let%expect_test _ =
  let glob = Glob.of_string "test" in
  printf "%S" (Glob.to_string glob);
  [%expect {| "test" |}]

let%expect_test _ =
  let glob = Glob.of_string "te*" in
  test glob "test";
  [%expect {| "te*" matches "test" == true |}];
  test glob "t";
  [%expect {| "te*" matches "t" == false |}];
  test glob "te";
  [%expect {| "te*" matches "te" == true |}]

let%expect_test _ =
  let glob = Glob.of_string "*st" in
  test glob "test";
  [%expect {| "*st" matches "test" == true |}];
  test glob "t";
  [%expect {| "*st" matches "t" == false |}];
  test glob "st";
  [%expect {| "*st" matches "st" == false |}]
