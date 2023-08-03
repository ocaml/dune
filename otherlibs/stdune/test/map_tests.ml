open Stdune
open Dune_tests_common

let () = init ()

(* Check that [of_alist_multi] groups elements in the right order *)
let%expect_test _ =
  let open Dyn in
  String.Map.of_list_multi [ "a", 1; "b", 1; "a", 2; "a", 3; "b", 2 ]
  |> String.Map.to_dyn (list int)
  |> print_dyn;
  [%expect {|
map { "a" : [ 1; 2; 3 ]; "b" : [ 1; 2 ] }
|}]
;;
