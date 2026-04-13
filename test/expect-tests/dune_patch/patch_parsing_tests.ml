open Stdune

let () = Dune_tests_common.init ()

(* Testing patch parsing *)

let to_dyn = Repr.to_dyn Dune_patch.For_tests.Patch.repr

let test p =
  Dune_patch.For_tests.patches_of_string p
  |> Dyn.list to_dyn
  |> Dyn.pp
  |> Format.printf "%a" Pp.to_fmt
;;

let%expect_test "patches_of_string - basic" =
  test Patch_examples.basic;
  [%expect {| [ { prefix = 1; op = Replace "foo.ml" } ] |}]
;;

let%expect_test "patches_of_string - subdirectory" =
  test Patch_examples.subdirectory;
  [%expect {| [ { prefix = 1; op = Replace "dir/foo.ml" } ] |}]
;;

let%expect_test "patches_of_string - combined" =
  test Patch_examples.combined;
  [%expect
    {|
    [ { prefix = 1; op = Replace "foo.ml" }
    ; { prefix = 1; op = Replace "dir/foo.ml" }
    ]
    |}]
;;

let%expect_test "patches_of_string - new_file" =
  test Patch_examples.new_file;
  [%expect {| [ { prefix = 1; op = New "foo.ml" } ] |}]
;;

let%expect_test "patches_of_string - delete_file" =
  test Patch_examples.delete_file;
  [%expect {| [ { prefix = 1; op = Delete "foo.ml" } ] |}]
;;

let%expect_test "patches_of_string - unified" =
  test Patch_examples.unified;
  [%expect {| [ { prefix = 1; op = Replace "foo.ml" } ] |}]
;;

let%expect_test "patches_of_string - no_prefix" =
  test Patch_examples.no_prefix;
  [%expect {| [ { prefix = 0; op = Replace "foo.ml" } ] |}]
;;

let%expect_test "patches_of_string - random_prefix" =
  test Patch_examples.random_prefix;
  [%expect {| [ { prefix = 1; op = Replace "foo.ml" } ] |}]
;;

let%expect_test "patches_of_string - spaces" =
  test Patch_examples.spaces;
  [%expect {| [ { prefix = 1; op = Replace "foo" } ] |}]
;;

let%expect_test "patches_of_string - unified_spaces" =
  test Patch_examples.unified_spaces;
  [%expect {| [ { prefix = 1; op = Replace "foo" } ] |}]
;;

let%expect_test "patches_of_string - hello_world" =
  test Patch_examples.hello_world;
  [%expect {| [ { prefix = 1; op = New "foo.ml" } ] |}]
;;

let%expect_test "patches_of_string - rename_patch" =
  test Patch_examples.rename_patch;
  [%expect {| [] |}]
;;

let%expect_test "patches_of_string - git_ext_delete_only" =
  test Patch_examples.git_ext_delete_only;
  [%expect {| [ { prefix = 1; op = Delete "foo.ml" } ] |}]
;;

let%expect_test "patches_of_string - git_ext_create_only" =
  test Patch_examples.git_ext_create_only;
  [%expect {| [ { prefix = 1; op = New "foo.ml" } ] |}]
;;

let%expect_test "patches_of_string - edit_with_rename" =
  test Patch_examples.edit_with_rename;
  [%expect {| [ { prefix = 0; op = Replace "b/target.ml" } ] |}]
;;
