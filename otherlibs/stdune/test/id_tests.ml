open Stdune

let () = Printexc.record_backtrace true

module Id = Id.Make ()

let id1 = Id.gen ()
let id2 = Id.gen ()
let id3 = Id.gen ()

let test table f =
  if f table
  then print_endline "[PASS]"
  else (
    print_endline "[FAIL]";
    Id.Hashset.to_dyn table |> Dyn.to_string |> print_endline)
;;

let%expect_test "basic set" =
  let set = Id.Hashset.create () in
  test set Id.Hashset.is_empty;
  [%expect {| [PASS] |}];
  test set (fun set -> not (Id.Hashset.mem set id1));
  [%expect {|
    [PASS] |}]
;;

let%expect_test "add 1 element" =
  let set = Id.Hashset.create () in
  Id.Hashset.add set id1;
  test set (fun set -> not (Id.Hashset.is_empty set));
  [%expect {|
    [PASS] |}];
  test set (fun set -> Id.Hashset.mem set id1);
  [%expect {|
    [PASS] |}];
  Id.Hashset.add set id1;
  test set (fun set -> Id.Hashset.mem set id1);
  [%expect {| [PASS] |}];
  Id.Hashset.add set id2;
  test set (fun set -> Id.Hashset.mem set id2);
  [%expect {| [PASS] |}];
  Id.Hashset.add set id3;
  test set (fun set -> Id.Hashset.mem set id3);
  [%expect {|
    [PASS] |}];
  test set (fun set ->
    List.for_all [ id1; id2; id3 ] ~f:(fun id -> Id.Hashset.mem set id));
  [%expect {| [PASS] |}]
;;
