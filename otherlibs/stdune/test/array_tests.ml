open Stdune
open Dyn
open Dune_tests_common

let () = init ()

module Map = String.Array.Map
module Set = String.Array.Set

let print_bool b = bool b |> print_dyn
let print_int_option x = option int x |> print_dyn
let print_map t = Map.to_list t |> list (pair string int) |> print_dyn
let print_set t = Set.to_list t |> list string |> print_dyn
let print_string s = string s |> print_dyn
let print_string_list l = list string l |> print_dyn

let print_duplicate_key_error () =
  match Map.of_list_exn [ "a", 1; "a", 2 ] with
  | _ -> print_endline "ok"
  | exception Code_error.E _ -> print_endline "duplicate key"
;;

let print_duplicate_sorted_key_error () =
  match Map.of_sorted_list_exn [ "a", 1; "a", 2 ] with
  | _ -> print_endline "ok"
  | exception Code_error.E _ -> print_endline "duplicate sorted key"
;;

let print_unsorted_map_error () =
  match Map.of_sorted_list_exn [ "b", 2; "a", 1 ] with
  | _ -> print_endline "ok"
  | exception Code_error.E _ -> print_endline "unsorted"
;;

let print_unsorted_set_error () =
  match Set.of_sorted_list [ "b"; "a" ] with
  | _ -> print_endline "ok"
  | exception Code_error.E _ -> print_endline "unsorted"
;;

let%expect_test "array-backed map" =
  let map = Map.empty in
  print_bool (Map.is_empty map);
  print_bool (Map.mem map "a");
  print_int_option (Map.find map "a");
  let map = Map.of_list_exn [ "b", 2; "a", 1 ] in
  print_bool (Map.is_empty map);
  print_bool (Map.mem map "a");
  print_bool (Map.mem map "c");
  print_int_option (Map.find map "a");
  print_int_option (Map.find map "b");
  print_int_option (Map.find map "c");
  print_map map;
  print_map (Map.of_sorted_list_exn [ "a", 1; "b", 2 ]);
  print_map (Map.union_left_biased map (Map.of_sorted_list_exn [ "b", 20; "c", 30 ]));
  print_map (Map.union_left_biased Map.empty map);
  print_map (Map.union_left_biased map map);
  print_bool (map == Map.union_left_biased map Map.empty);
  print_map (Map.filter_mapi map ~f:(fun key value -> Some (value + String.length key)));
  print_map
    (Map.filter_mapi map ~f:(fun key value ->
       if String.equal key "a" then Some (value + 10) else None));
  print_map (Map.filter_mapi map ~f:(fun _ _ -> None));
  print_set (Map.keys map);
  Map.to_list_map map ~f:(fun key value -> sprintf "%s:%d" key value) |> print_string_list;
  let bindings = ref [] in
  Map.iteri map ~f:(fun key value -> bindings := sprintf "%s=%d" key value :: !bindings);
  List.rev !bindings |> print_string_list;
  print_bool (Map.equal map (Map.of_list_exn [ "a", 1; "b", 2 ]) ~equal:Int.equal);
  print_bool (Map.equal map (Map.of_list_exn [ "a", 1; "b", 3 ]) ~equal:Int.equal);
  print_duplicate_key_error ();
  print_duplicate_sorted_key_error ();
  print_unsorted_map_error ();
  [%expect
    {|
true
false
None
false
true
false
Some 1
Some 2
None
[ ("a", 1); ("b", 2) ]
[ ("a", 1); ("b", 2) ]
[ ("a", 1); ("b", 2); ("c", 30) ]
[ ("a", 1); ("b", 2) ]
[ ("a", 1); ("b", 2) ]
true
[ ("a", 2); ("b", 3) ]
[ ("a", 11) ]
[]
[ "a"; "b" ]
[ "a:1"; "b:2" ]
[ "a=1"; "b=2" ]
true
false
duplicate key
duplicate sorted key
unsorted
|}]
;;

let%expect_test "array-backed set" =
  let empty = Set.empty in
  print_bool (Set.is_empty empty);
  print_bool (Set.mem empty "a");
  let set = Set.of_list [ "b"; "a"; "b"; "d" ] in
  print_set set;
  print_set (Set.union set (Set.of_list [ "c"; "d" ]));
  print_bool (Set.is_subset (Set.of_list [ "a"; "d" ]) ~of_:set);
  print_bool (Set.is_subset (Set.of_list [ "a"; "c" ]) ~of_:set);
  print_bool (Set.are_disjoint set (Set.of_list [ "c"; "e" ]));
  print_bool (Set.are_disjoint set (Set.of_list [ "c"; "d" ]));
  print_set (Set.diff set (Set.of_list [ "b" ]));
  print_set (Set.diff set (Set.of_list [ "a"; "d"; "x" ]));
  print_set (Set.diff set set);
  print_bool (set == Set.diff set Set.empty);
  print_bool (set == Set.diff set (Set.of_list [ "c" ]));
  print_set (Set.filter set ~f:(fun s -> not (String.equal s "b")));
  print_bool (set == Set.filter set ~f:(fun _ -> true));
  print_set (Set.of_sorted_list [ "a"; "b"; "b"; "d" ]);
  print_unsorted_set_error ();
  Set.to_list_map set ~f:Fun.id |> print_string_list;
  Set.fold set ~init:"" ~f:(fun s acc -> acc ^ s) |> print_string;
  print_bool (Set.is_empty set);
  print_bool (Set.mem set "a");
  print_bool (Set.mem set "c");
  print_bool (Set.equal set (Set.of_list [ "d"; "b"; "a" ]));
  print_bool (Set.equal set (Set.of_list [ "a"; "b" ]));
  [%expect
    {|
true
false
[ "a"; "b"; "d" ]
[ "a"; "b"; "c"; "d" ]
true
false
true
false
[ "a"; "d" ]
[ "b" ]
[]
true
true
[ "a"; "d" ]
true
[ "a"; "b"; "d" ]
unsorted
[ "a"; "b"; "d" ]
"abd"
false
true
false
true
false
|}]
;;
