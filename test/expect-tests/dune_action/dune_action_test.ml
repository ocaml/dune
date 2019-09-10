open Dune_action

let%expect_test _ =
  try ignore @@ Path.of_string "/some/absolute/path"
  with Invalid_argument message ->
    print_endline message;
    [%expect
      {| Path "/some/absolute/path" is absolute. All paths used with dune-action-plugin must be relative. |}]

let%expect_test _ =
  let action =
    read_file ~path:(Path.of_string "foo_dir/foo") |> map ~f:print_endline
  in
  Private.do_run action;
  [%expect {|
    Hello from foo!
  |}]

let%expect_test _ =
  let action =
    read_directory ~path:(Path.of_string "foo_dir")
    |> map ~f:(fun data -> String.concat "," data |> print_endline)
  in
  Private.do_run action;
  [%expect {|
    foo
  |}]

let%expect_test _ =
  let action = write_file ~path:(Path.of_string "bar") ~data:"foo" in
  Private.do_run action;
  [%expect {| |}]

let run_action_expect_throws action =
  try
    Private.do_run action;
    print_endline "SHOULD BE UNREACHABLE"
  with Private.Execution_error.E message -> print_endline message

let%expect_test _ =
  let action =
    read_file ~path:(Path.of_string "file_that_does_not_exist")
    |> map ~f:ignore
  in
  run_action_expect_throws action;
  [%expect
    {|
    read_file: file_that_does_not_exist: No such file or directory
  |}]

let%expect_test _ =
  let action =
    read_directory ~path:(Path.of_string "directory_that_does_not_exist")
    |> map ~f:ignore
  in
  run_action_expect_throws action;
  [%expect {|
    read_directory: No such file or directory
  |}]

let%expect_test _ =
  let action =
    write_file
      ~path:(Path.of_string "directory_that_does_not_exist/foo")
      ~data:"foo"
  in
  run_action_expect_throws action;
  [%expect
    {|
    write_file: directory_that_does_not_exist/foo: No such file or directory
  |}]
