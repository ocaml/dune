open! Stdune
open Dune_tests_common

let touch file = Io.write_file (Path.of_string file) ""
let mkdir x = Fpath.mkdir x |> Fpath.dyn_of_mkdir_result |> print_dyn
let mkdir_p x = Fpath.mkdir_p x |> Fpath.dyn_of_mkdir_p_result |> print_dyn

(* creates a temp enviornment for testing *)
let test f =
  let tmp = Temp.create Dir ~prefix:"dune-test-fpath" ~suffix:"" in
  let cwd = Sys.getcwd () in
  Sys.chdir (Path.to_string tmp);
  f ();
  Sys.chdir cwd;
  Temp.destroy Dir tmp
;;

(* Directory creation in current working directory *)

let mkdir_in_current_dir mkdir = test @@ fun () -> mkdir "a"

let%expect_test "mkdir" =
  mkdir_in_current_dir mkdir;
  [%expect {|
  Created
  |}]
;;

let%expect_test "mkdir_p" =
  mkdir_in_current_dir mkdir;
  [%expect {|
  Created
  |}]
;;

(* Directory creation of existing directory *)

let mkdir_existing_dir mkdir =
  test
  @@ fun () ->
  mkdir "b";
  mkdir "b"
;;

let%expect_test "mkdir on existing dir" =
  mkdir_existing_dir mkdir;
  [%expect {|
  Created
  Already_exists
  |}]
;;

let%expect_test "mkdir_p on existing dir" =
  mkdir_existing_dir mkdir_p;
  [%expect {|
  Created
  Already_exists
  |}]
;;

(* Directory creation of existing non-dir file *)

let mkdir_existing_file mkdir =
  test
  @@ fun () ->
  touch "d";
  mkdir "d"
;;

let%expect_test "mkdir on existing non-dir file" =
  mkdir_existing_file mkdir;
  [%expect {|
  Already_exists_not_directory
  |}]
;;

let%expect_test "mkdir_p on existing non-dir file" =
  mkdir_existing_file mkdir_p;
  [%expect {|
  Already_exists_not_directory "d"
  |}]
;;

(* Directory creation when parent is a non-directory*)

let mkdir_parent_non_dir mkdir =
  test
  @@ fun () ->
  touch "c";
  mkdir "c";
  mkdir "c/a";
  mkdir "c/a/a";
  mkdir "c/a/a/a"
;;

let%expect_test "mkdir with non-dir parent" =
  mkdir_parent_non_dir mkdir;
  [%expect
    {|
  Already_exists_not_directory
  Parent_not_directory
  Parent_not_directory
  Parent_not_directory
  |}]
;;

let%expect_test "mkdir_p with non-dir parent" =
  mkdir_parent_non_dir mkdir_p;
  [%expect
    {|
  Already_exists_not_directory "c"
  Already_exists_not_directory "c"
  Already_exists_not_directory "c"
  Already_exists_not_directory "c"
  |}]
;;
