open Stdune
open Dune_tests_common

let () = init ()

(* Check that [of_alist_multi] groups elements in the right order *)
let%expect_test _ =
  let open Dyn in
  String.Map.of_list_multi [ "a", 1; "b", 1; "a", 2; "a", 3; "b", 2 ]
  |> String.Map.to_dyn (list int)
  |> print_dyn;
  [%expect
    {|
map { "a" : [ 1; 2; 3 ]; "b" : [ 1; 2 ] }
|}]
;;

let print_int_map map = String.Map.to_dyn Dyn.int map |> print_dyn

let%expect_test "list reductions preserve input order" =
  let bindings = [ "a", 1; "b", 2; "a", 3 ] in
  String.Map.of_list_reduce bindings ~f:(fun before after -> (before * 10) + after)
  |> print_int_map;
  String.Map.of_list_reducei bindings ~f:(fun key before after ->
    String.length key + (before * 10) + after)
  |> print_int_map;
  String.Map.of_list_fold bindings ~init:4 ~f:(fun before after -> (before * 10) + after)
  |> print_int_map;
  [%expect
    {|
    map { "a" : 13; "b" : 2 }
    map { "a" : 14; "b" : 2 }
    map { "a" : 413; "b" : 42 }
    |}]
;;
