open Stdune

let () = Dune_tests_common.init ()

(* Testing patch application *)

let test_apply_patches patches files check_files =
  Temp.with_temp_dir
    ~parent_dir:(Path.of_string ".")
    ~prefix:"dune"
    ~suffix:"apply_test"
    ~f:(fun dir ->
      let dir = Result.ok_exn dir in
      List.iter files ~f:(fun (filename, contents) ->
        let target = Path.relative dir filename in
        Option.iter (Path.parent target) ~f:(fun parent ->
          ignore (Fpath.mkdir_p (Path.to_string parent)));
        Io.write_file target contents);
      Dune_patch.For_tests.apply_patches ~dir patches;
      List.iter check_files ~f:(fun filename ->
        let target = Path.relative dir filename in
        if Fpath.exists (Path.to_string target)
        then (
          let contents = Io.read_file target in
          Printf.printf "%s:\n%s" filename contents)
        else Printf.printf "%s: NOT FOUND\n" filename))
;;

let%expect_test "apply_patches - basic" =
  let patches = Dune_patch.For_tests.parse_patches ~loc:Loc.none Patch_examples.basic in
  test_apply_patches patches [ "foo.ml", "This is wrong\n" ] [ "foo.ml" ];
  [%expect
    {|
    foo.ml:
    This is right |}]
;;

let%expect_test "apply_patches - new_file" =
  let patches =
    Dune_patch.For_tests.parse_patches ~loc:Loc.none Patch_examples.new_file
  in
  test_apply_patches patches [] [ "foo.ml" ];
  [%expect
    {|
    foo.ml:
    This is right
    |}]
;;

let%expect_test "apply_patches - delete_file" =
  let patches =
    Dune_patch.For_tests.parse_patches ~loc:Loc.none Patch_examples.delete_file
  in
  test_apply_patches patches [ "foo.ml", "This is wrong\n" ] [ "foo.ml" ];
  [%expect {| foo.ml: NOT FOUND |}]
;;

let%expect_test "apply_patches - missing file for edit" =
  let patches = Dune_patch.For_tests.parse_patches ~loc:Loc.none Patch_examples.basic in
  test_apply_patches patches [] [ "foo.ml" ];
  [%expect.unreachable]
[@@expect.uncaught_exn
  {| ("Error: Cannot edit file \"foo.ml\": file does not exist\n") |}]
;;

let%expect_test "apply_patches - content mismatch" =
  let patches = Dune_patch.For_tests.parse_patches ~loc:Loc.none Patch_examples.basic in
  test_apply_patches patches [ "foo.ml", "Something else entirely\n" ] [ "foo.ml" ];
  [%expect.unreachable]
[@@expect.uncaught_exn
  {|
  ( "Error: Patch could not be applied to \"foo.ml\": hunk does not match file\
   \ncontents\
   \n")
  |}]
;;

let%expect_test "apply_patches - create over existing file" =
  let patches =
    Dune_patch.For_tests.parse_patches ~loc:Loc.none Patch_examples.new_file
  in
  test_apply_patches patches [ "foo.ml", "Old content\n" ] [ "foo.ml" ];
  [%expect
    {|
    foo.ml:
    This is right
    |}]
;;

let%expect_test "apply_patches - delete missing file" =
  let patches =
    Dune_patch.For_tests.parse_patches ~loc:Loc.none Patch_examples.delete_file
  in
  test_apply_patches patches [] [ "foo.ml" ];
  [%expect {| foo.ml: NOT FOUND |}]
;;

(* Edit with rename now works correctly after prefix parsing fix. *)
let%expect_test "apply_patches - edit_with_rename" =
  let patches =
    Dune_patch.For_tests.parse_patches ~loc:Loc.none Patch_examples.edit_with_rename
  in
  test_apply_patches patches [ "source.ml", "This is wrong\n" ] [ "target.ml" ];
  [%expect
    {|
    target.ml:
    This is right
    |}]
;;

let%expect_test "apply_patches - git_ext_delete_only (parses as Delete)" =
  let patches =
    Dune_patch.For_tests.parse_patches ~loc:Loc.none Patch_examples.git_ext_delete_only
  in
  test_apply_patches patches [ "foo.ml", "Hello World\n" ] [ "foo.ml" ];
  [%expect {| foo.ml: NOT FOUND |}]
;;

let%expect_test "apply_patches - git_ext_create_only (parses as Create)" =
  let patches =
    Dune_patch.For_tests.parse_patches ~loc:Loc.none Patch_examples.git_ext_create_only
  in
  test_apply_patches patches [] [ "foo.ml" ];
  [%expect
    {|
    foo.ml:
    Hello World |}]
;;

(* Spurious "new file mode" on an edit - the ---/+++ headers take precedence
   and the patch is applied as an Edit. *)
let%expect_test "apply_patches - spurious new file mode" =
  let patches =
    Dune_patch.For_tests.parse_patches ~loc:Loc.none Patch_examples.spurious_new_file_mode
  in
  test_apply_patches patches [ "dir/baz.ml", "This is wrong\n" ] [ "dir/baz.ml" ];
  [%expect
    {|
    dir/baz.ml:
    This is right
    |}]
;;

(* Combined patch where second entry has spurious "new file mode" -
   the ---/+++ headers take precedence and both patches apply correctly. *)
let%expect_test "apply_patches - combined with spurious new file mode" =
  let patches =
    Dune_patch.For_tests.parse_patches
      ~loc:Loc.none
      Patch_examples.combined_with_spurious_new_file_mode
  in
  test_apply_patches
    patches
    [ "bar.ml", "This is wrong\n"; "dir/baz.ml", "This is wrong\n" ]
    [ "bar.ml"; "dir/baz.ml" ];
  [%expect
    {|
    bar.ml:
    This is right
    dir/baz.ml:
    This is right
    |}]
;;

(* Rename operations now work correctly after fixing prefix parsing. *)
let%expect_test "apply_patches - rename_patch" =
  let patches =
    Dune_patch.For_tests.parse_patches ~loc:Loc.none Patch_examples.rename_patch
  in
  test_apply_patches patches [ "old.ml", "content\n" ] [ "new.ml" ];
  [%expect
    {|
    new.ml:
    content
    |}]
;;
