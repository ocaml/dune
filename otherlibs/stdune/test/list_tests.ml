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
