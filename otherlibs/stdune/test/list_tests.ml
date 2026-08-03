open Stdune
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

let%expect_test "List.hash distinguishes length and identity" =
  (* Bool.hash false = 0, so under any rolling hash that starts at 0,
     [], [false], [false; false], ... would all collide. The current
     implementation starts at 1, breaking that family of collisions. *)
  let h = List.hash Bool.hash in
  let print xs = Printf.printf "%d\n" (h xs) in
  print [];
  print [ false ];
  print [ false; false ];
  print [ false; false; false ];
  print [ true ];
  print [ true; false ];
  print [ false; true ];
  [%expect
    {|
    1
    31
    961
    29791
    32
    992
    962
    |}]
;;
