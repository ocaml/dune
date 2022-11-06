open! Stdune
open Dune_tests_common

let touch file = Io.write_file (Path.of_string file) ""

let%expect_test "Fpath.mkdir_p" =
  Fpath.mkdir_p "a" |> Fpath.dyn_of_mkdir_p_result |> print_dyn;
  [%expect {|
  Created
  |}]

let%expect_test "Fpath.mkdir_p on existing dir" =
  Fpath.mkdir_p "b" |> Fpath.dyn_of_mkdir_p_result |> print_dyn;
  Fpath.mkdir_p "b" |> Fpath.dyn_of_mkdir_p_result |> print_dyn;
  [%expect {|
  Created
  Already_exists
  |}]

let%expect_test "Fpath.mkdir_p with non-dir parent" =
  touch "c";
  Fpath.mkdir_p "c/a" |> Fpath.dyn_of_mkdir_p_result |> print_dyn;
  Fpath.mkdir_p "c/a/a" |> Fpath.dyn_of_mkdir_p_result |> print_dyn;
  Fpath.mkdir_p "c/a/a/a" |> Fpath.dyn_of_mkdir_p_result |> print_dyn;
  [%expect
    {|
  Already_exists_not_directory "c"
  Already_exists_not_directory "c"
  Already_exists_not_directory "c"
  |}]

let%expect_test "Fpath.mkdir_p on existing non-dir file" =
  touch "d";
  Fpath.mkdir_p "d" |> Fpath.dyn_of_mkdir_p_result |> print_dyn;
  [%expect {|
  Already_exists_not_directory "d"
  |}]
