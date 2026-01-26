module Buffer = Dune_trace.Private.Buffer

let b () = Buffer.create 100

let test buf f =
  let pre = Buffer.to_string buf in
  f buf;
  let post = Buffer.to_string buf in
  Printf.printf
    "%S -> %S available = %d pos = %d max_size = %d\n"
    pre
    post
    (Buffer.available buf)
    (Buffer.pos buf)
    (Buffer.max_size buf)
;;

let%expect_test "empty" =
  test (b ()) (fun (_ : Buffer.t) -> ());
  [%expect {| "" -> "" available = 100 pos = 0 max_size = 100 |}]
;;

let%expect_test "add_char" =
  let b = b () in
  test b (fun b -> Buffer.add_char b 'a');
  [%expect {| "" -> "a" available = 99 pos = 1 max_size = 100 |}];
  test b (fun b -> Buffer.add_char b 'b');
  [%expect {| "a" -> "ab" available = 98 pos = 2 max_size = 100 |}];
  test b (fun b -> Buffer.add_char b 'c');
  [%expect {| "ab" -> "abc" available = 97 pos = 3 max_size = 100 |}]
;;

let%expect_test "add_string" =
  let b = b () in
  test b (fun b -> Buffer.add_string b "aaa");
  [%expect {| "" -> "aaa" available = 97 pos = 3 max_size = 100 |}];
  test b (fun b -> Buffer.add_string b "");
  [%expect {| "aaa" -> "aaa" available = 97 pos = 3 max_size = 100 |}];
  test b (fun b -> Buffer.add_string b "bbb");
  [%expect {| "aaa" -> "aaabbb" available = 94 pos = 6 max_size = 100 |}]
;;

let%expect_test "clear" =
  let b = b () in
  Buffer.add_string b "abcde";
  test b Buffer.clear;
  [%expect {| "abcde" -> "" available = 100 pos = 0 max_size = 100 |}]
;;

let%expect_test "drop" =
  let b = b () in
  Buffer.add_string b "abcde";
  test b (fun b -> Buffer.drop b 1);
  [%expect {| "abcde" -> "bcde" available = 96 pos = 4 max_size = 100 |}];
  test b (fun b -> Buffer.drop b 3);
  [%expect {| "bcde" -> "e" available = 99 pos = 1 max_size = 100 |}]
;;

let%expect_test "resize" =
  let b = b () in
  Buffer.add_string b "abcde";
  test b (fun b -> Buffer.resize b 50);
  [%expect {| "abcde" -> "abcde" available = 45 pos = 5 max_size = 50 |}];
  test b (fun b -> Buffer.resize b 10);
  [%expect {| "abcde" -> "abcde" available = 5 pos = 5 max_size = 10 |}]
;;
