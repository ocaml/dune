let printf = Printf.printf

let test string =
  printf "should_exclude(%s) = %s\n" string
    (Dune_file_watcher.For_tests.should_exclude string |> Bool.to_string)

let%expect_test _ =
  test "file.ml";
  test "dir/file.ml";
  test "4913";
  test "dir/4913";
  test "4913.ml";
  test "84913";
  test "_opam";
  test "dir/_opam";
  test "this_is_not_opam";
  test "#file#";
  test "dir/#file#";
  test "dir/#subdir#/file";
  test ".#file";
  test "dir/.#file";
  test "dir/.#subdir/file";
  [%expect
    {|
    should_exclude(file.ml) = false
    should_exclude(dir/file.ml) = false
    should_exclude(4913) = true
    should_exclude(dir/4913) = true
    should_exclude(4913.ml) = false
    should_exclude(84913) = false
    should_exclude(_opam) = true
    should_exclude(dir/_opam) = true
    should_exclude(this_is_not_opam) = false
    should_exclude(#file#) = true
    should_exclude(dir/#file#) = true
    should_exclude(dir/#subdir#/file) = false
    should_exclude(.#file) = true
    should_exclude(dir/.#file) = true
    should_exclude(dir/.#subdir/file) = true
  |}]
