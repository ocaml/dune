open Stdune
open Dune_tests_common

let () = init ()

let%expect_test "filenames reject platform directory separators" =
  let windows = Sys.win32 || Sys.cygwin in
  let check_filename s ~expected =
    let actual = Option.is_some (Filename.of_string s) in
    Printf.printf "filename %S: %b\n" s (Bool.equal actual expected)
  in
  let check_extension s ~expected =
    let actual = Option.is_some (Filename.Extension.of_string s) in
    Printf.printf "extension %S: %b\n" s (Bool.equal actual expected)
  in
  check_filename "a/b" ~expected:false;
  check_filename "a\\b" ~expected:(not windows);
  check_filename "a:b" ~expected:(not windows);
  check_extension ".a/b" ~expected:false;
  check_extension ".a\\b" ~expected:(not windows);
  check_extension ".a:b" ~expected:(not windows);
  [%expect
    {|
    filename "a/b": true
    filename "a\\b": true
    filename "a:b": true
    extension ".a/b": true
    extension ".a\\b": true
    extension ".a:b": true
    |}]
;;

let extension s =
  let ext =
    Filename.of_string s
    |> Option.map ~f:Filename.extension
    |> Option.value ~default:(Path.extension (Path.of_string s))
    |> Filename.Extension.Or_empty.to_string
  in
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

let extension_to_filename s =
  match Filename.Extension.of_string_exn s |> Filename.Extension.to_filename with
  | fn -> Printf.printf "%S -> %S\n" s (Filename.to_string fn)
  | exception Code_error.E _ -> Printf.printf "%S -> invalid\n" s
;;

let%expect_test "extension to_filename validates filename invariants" =
  List.iter [ "."; ".ml" ] ~f:extension_to_filename;
  [%expect
    {|
"." -> invalid
".ml" -> ".ml"
|}]
;;
