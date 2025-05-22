open! Stdune
open Dyn
open Dune_tests_common

let intersperse t ~sep = List.intersperse t ~sep |> list string |> print_dyn

let%expect_test _ =
  intersperse [] ~sep:"sep";
  [%expect {| [] |}]
;;

let%expect_test _ =
  intersperse [ "foo" ] ~sep:"sep";
  [%expect {| [ "foo" ] |}]
;;

let%expect_test _ =
  intersperse [ "foo"; "bar"; "baz" ] ~sep:"sep";
  [%expect {| [ "foo"; "sep"; "bar"; "sep"; "baz" ] |}]
;;

let deduplicate t = List.deduplicate t ~equal:String.equal |> list string |> print_dyn

let%expect_test _ =
  deduplicate [];
  [%expect {| [] |}]
;;

let%expect_test _ =
  deduplicate [ "foo" ];
  [%expect {| [ "foo" ] |}]
;;

let%expect_test _ =
  deduplicate [ "foo"; "foo" ];
  [%expect {| [ "foo" ] |}]
;;

let%expect_test _ =
  deduplicate [ "foo"; "bar"; "foo" ];
  [%expect {| [ "foo"; "bar" ] |}]
;;

let%expect_test _ =
  deduplicate
    [ "foo"; "bar"; "foo"; "foo"; "foo"; "bar"; "foo"; "baz"; "foo"; "bar"; "qux" ];
  [%expect {| [ "foo"; "bar"; "baz"; "qux" ] |}]
;;
