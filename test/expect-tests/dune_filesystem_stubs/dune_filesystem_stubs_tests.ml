open Stdune

let%expect_test "" =
  Io.String_path.write_file "foo1" "";
  Io.String_path.write_file "foo2" "";
  (match Dune_filesystem_stubs.read_directory_with_kinds "." with
  | Error e -> printfn "error: %s" (Unix.error_message e)
  | Ok s -> List.iter s ~f:(fun (name, _) -> print_endline name));
  [%expect {|
    foo1
    dune_filesystem_stubs_tests.ml
    foo2
    .dune_filesystem_stubs_test.inline-tests |}]
