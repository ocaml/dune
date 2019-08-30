open Stdune
open Dune_action

let ident x = x

let map2 ~ok ~err = function
  | Ok a -> ok a
  | Error a -> err a

let%expect_test _ =
  let action =
    read_file ~path:(Path.of_string "foo_dir/foo")
    |> map ~f:(function
      | Ok data -> print_endline data
        | Error _ -> print_endline "SHOULD NOT BE PRINTED")
  in
  run action;
  [%expect
    {|
    Hello from foo!
  |}]

let%expect_test _ =
  let action =
    read_directory ~path:(Path.of_string "foo_dir")
    |> map ~f:(function
      | Ok data -> print_endline (String.concat ~sep:"," data)
        | Error _ -> print_endline "SHOULD NOT BE PRINTED")
  in
  run action;
  [%expect {|
    foo
  |}]

let%expect_test _ =
  let action =
    write_file ~path:(Path.of_string "bar") ~data:"foo"
    |> map ~f:(function
      | Ok () -> ()
        | Error _ -> print_endline "SHOULD NOT BE PRINTED")
  in
  run action;
  [%expect {| |}]

let%expect_test _ =
  let action =
    read_file ~path:(Path.of_string "file_that_does_not_exist")
    |> map ~f:(function
      | Ok _ -> print_endline "SHOULD NOT BE PRINTED"
        | Error error -> print_endline error)
  in
  run action;
  [%expect
    {|
    read_file: file_that_does_not_exist: No such file or directory
  |}]

let%expect_test _ =
  let action =
    read_directory ~path:(Path.of_string "directory_that_does_not_exist")
    |> map ~f:(function
      | Ok _ -> print_endline "SHOULD NOT BE PRINTED"
        | Error error -> print_endline error)
  in
  run action;
  [%expect {|
    read_directory: No such file or directory
  |}]

let%expect_test _ =
  let action =
    write_file
      ~path:(Path.of_string "directory_that_does_not_exist/foo")
      ~data:"foo"
    |> map ~f:(function
      | Ok _ -> print_endline "SHOULD NOT BE PRINTED"
        | Error error -> print_endline error)
  in
  run action;
  [%expect {|
    write_file: directory_that_does_not_exist/foo: No such file or directory
  |}]
