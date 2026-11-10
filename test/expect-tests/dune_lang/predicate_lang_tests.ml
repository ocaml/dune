open Stdune
open Dune_tests_common
module Glob = Predicate_lang.Glob

let () = init ()

let matches predicate names =
  List.map names ~f:(fun name ->
    name, Glob.test predicate ~standard:Predicate_lang.false_ name)
  |> Dyn.(list (pair string bool))
  |> print_dyn
;;

let%expect_test "literal filename sets" =
  let predicate = Glob.of_string_set (String.Set.of_list [ "*"; "?"; "[x]"; "a" ]) in
  matches predicate [ "*"; "?"; "[x]"; "a"; "x"; "b"; "" ];
  [%expect
    {|
    [ ("*", true)
    ; ("?", true)
    ; ("[x]", true)
    ; ("a", true)
    ; ("x", false)
    ; ("b", false)
    ; ("", false)
    ]
    |}];
  print_dyn (Glob.to_dyn predicate);
  [%expect {| Literals [ "*"; "?"; "[x]"; "a" ] |}]
;;

let%expect_test "empty sets and complement" =
  let empty = Glob.of_string_set String.Set.empty in
  matches empty [ ""; "a" ];
  [%expect {| [ ("", false); ("a", false) ] |}];
  matches (Predicate_lang.not empty) [ ""; "a" ];
  [%expect {| [ ("", true); ("a", true) ] |}];
  let names = Glob.of_string_set (String.Set.of_list [ "META"; "plugin.cmxs" ]) in
  matches (Predicate_lang.not names) [ "META"; "plugin.cmxs"; "proof.vo" ];
  [%expect {| [ ("META", false); ("plugin.cmxs", false); ("proof.vo", true) ] |}]
;;

let%expect_test "construction order does not affect equality or hashing" =
  let names = [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i" ] in
  let forward = List.fold_left names ~init:String.Set.empty ~f:String.Set.add in
  let backward =
    List.fold_left (List.rev names) ~init:String.Set.empty ~f:String.Set.add
  in
  let a = Glob.of_string_set forward in
  let b = Glob.of_string_set backward in
  print_dyn
    (Dyn.record
       [ "equal", Dyn.bool (Glob.equal a b)
       ; "same_order", Dyn.bool (Ordering.is_eq (Glob.compare a b))
       ; "same_hash", Dyn.bool (Glob.hash a = Glob.hash b)
       ; "same_digest", Dyn.bool (Dune_digest.equal (Glob.digest a) (Glob.digest b))
       ]);
  [%expect
    {|
    { equal = true; same_order = true; same_hash = true; same_digest = true }
    |}]
;;
