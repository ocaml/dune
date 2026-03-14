open Stdune
open Dune_tests_common

let () = init ()

let parse file contents =
  Dune_rules.Ocamldep.parse_output ~file:(Path.of_string file) contents
  |> Dyn.list Dyn.string
  |> print_dyn
;;

let%expect_test "parse_output handles paths and whitespace" =
  parse "/tmp/foo.ml" "sub/dir/foo.ml: Bar\tBaz  Qux\r\n";
  [%expect {| [ "Bar"; "Baz"; "Qux" ] |}]
;;

let%expect_test "parse_output handles files with no dependencies" =
  parse "foo.ml" "foo.ml:\n";
  [%expect {| [] |}]
;;
