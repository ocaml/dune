open! Stdune
open Dyn
open Dune_tests_common

let () = init ()

let take s n = String.take s n |> string |> print_dyn

let drop s n = String.drop s n |> string |> print_dyn

let split_n s n = String.split_n s n |> pair string string |> print_dyn

let%expect_test _ =
  take "foobar" 3;
  [%expect {|
"foo"
|}]

let%expect_test _ =
  take "foobar" 0;
  [%expect {|
""
|}]

let%expect_test _ =
  take "foo" 10;
  [%expect {|
"foo"
|}]

let%expect_test _ =
  take "" 10;
  [%expect {|
""
|}]

let%expect_test _ =
  take "" 0;
  [%expect {|
""
|}]

let%expect_test _ =
  drop "" 0;
  [%expect {|
""
|}]

let%expect_test _ =
  drop "foo" 0;
  [%expect {|
"foo"
|}]

let%expect_test _ =
  drop "foo" 5;
  [%expect {|
""
|}]

let%expect_test _ =
  drop "foobar" 3;
  [%expect {|
"bar"
|}]

let%expect_test _ =
  split_n "foobar" 3;
  [%expect {|
("foo", "bar")
|}]

let%expect_test _ =
  split_n "foobar" 10;
  [%expect {|
("foobar", "")
|}]

let%expect_test _ =
  split_n "foobar" 0;
  [%expect {|
("", "foobar")
|}]

let%expect_test _ =
  split_n "foobar" 6;
  [%expect {|
("foobar", "")
|}]

let%expect_test _ =
  split_n "" 0;
  [%expect {|
("", "")
|}]

let%expect_test _ =
  split_n "" 10;
  [%expect {|
("", "")
|}]

let%expect_test _ =
  String.longest_prefix [ "food"; "foo"; "foo-bar" ] |> string |> print_dyn;
  [%expect {|
"foo"
|}]

let%expect_test _ =
  String.drop_suffix "foobar" ~suffix:"bar" |> option string |> print_dyn;
  [%expect {|
Some "foo"
|}]

let%expect_test _ =
  String.drop_suffix "foobar" ~suffix:"foobar" |> option string |> print_dyn;
  [%expect {|
Some ""
|}]

let%expect_test _ =
  String.drop_suffix "foobar" ~suffix:"" |> option string |> print_dyn;
  [%expect {|
Some "foobar"
|}]

let%expect_test _ =
  String.drop_suffix "foobar" ~suffix:"foo" |> option string |> print_dyn;
  [%expect {|
None
|}]
