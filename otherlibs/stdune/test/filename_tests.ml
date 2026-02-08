open Stdune
open Dune_tests_common

let () = init ()

let extension s =
  let ext = Filename.extension s |> Filename.Extension.Or_empty.to_string in
  print (Pp.text ext)
;;

let%expect_test _ =
  extension "toto.titi";
  [%expect
    {|
.titi
|}]
;;

let%expect_test _ =
  extension "toto.";
  [%expect
    {|
.
|}]
;;

let%expect_test _ =
  extension ".";
  [%expect {| |}]
;;

let%expect_test _ =
  extension ".titi";
  [%expect {| |}]
;;

let%expect_test _ =
  extension ".a";
  [%expect {| |}]
;;

let%expect_test _ =
  extension "a.";
  [%expect
    {|
.
|}]
;;

let%expect_test _ =
  extension "a.a";
  [%expect
    {|
.a
|}]
;;

let%expect_test _ =
  extension "truc/a.a";
  [%expect
    {|
.a
|}]
;;

let%expect_test _ =
  extension "truc/.a";
  [%expect {| |}]
;;

let%expect_test _ =
  extension "truc/a.";
  [%expect
    {|
.
|}]
;;
