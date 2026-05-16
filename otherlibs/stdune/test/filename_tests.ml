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

let or_empty_extension_of_string_exn s =
  match Filename.Extension.Or_empty.of_string_exn s with
  | ext -> Printf.printf "%S -> %S\n" s (Filename.Extension.Or_empty.to_string ext)
  | exception Code_error.E _ -> Printf.printf "%S -> invalid\n" s
;;

let%expect_test "extension or_empty validates non-empty extensions" =
  List.iter
    [ ""; "."; ".ml"; ".tar.gz"; "ml"; ".foo/bar"; "./foo" ]
    ~f:or_empty_extension_of_string_exn;
  [%expect
    {|
"" -> ""
"." -> "."
".ml" -> ".ml"
".tar.gz" -> ".tar.gz"
"ml" -> invalid
".foo/bar" -> invalid
"./foo" -> invalid
|}]
;;
