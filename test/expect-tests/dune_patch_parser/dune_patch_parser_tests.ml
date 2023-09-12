open Stdune

let () = Dune_tests_common.init ()

let basic =
  {|
diff --git a/foo.ml b/foo.ml
index b69a69a5a..ea988f6bd 100644
--- a/foo.ml
+++ b/foo.ml
@@ -1,2 +1,2 @@
-This is wrong
+This is right
|}
;;

let subdirectory =
  {|
diff --git a/dir/foo.ml b/dir/foo.ml
index b69a69a5a..ea988f6bd 100644
--- a/dir/foo.ml
+++ b/dir/foo.ml
@@ -1,2 +1,2 @@
-This is wrong
+This is right
|}
;;

let test input =
  Dune_patch_parser.files_of_patch ~dir:(Path.of_string ".") input
  |> Dyn.list Path.to_dyn
  |> Dyn.to_string
  |> print_endline
;;

let%expect_test "basic parsing of patches" =
  test basic;
  [%expect {|
    [ In_source_tree "foo.ml" ] |}]
;;

let%expect_test "parsing files in subdirectories" =
  test subdirectory;
  [%expect {|
    [ In_source_tree "dir/foo.ml" ] |}]
;;
