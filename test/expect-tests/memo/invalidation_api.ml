open! Stdune
open Test_helpers.Make ()
module Reason = Memo.Invalidation.Reason

(* [clear_caches] and [changed_paths] are public-only Invalidation features, so they are
   exercised only here. *)

let%expect_test "clear_caches forces every memoized table to recompute" =
  let fa =
    Memo.create
      "a"
      ~input:(module Int)
      (fun i ->
         printfn "compute a %d" i;
         Memo.return (i + 1))
  in
  let fb =
    Memo.create
      "b"
      ~input:(module Int)
      (fun i ->
         printfn "compute b %d" i;
         Memo.return (i * 2))
  in
  let run_both () =
    printfn "a 1 = %d" (run_memo fa 1);
    printfn "b 2 = %d" (run_memo fb 2)
  in
  run_both ();
  [%expect
    {|
    compute a 1
    a 1 = 2
    compute b 2
    b 2 = 4
    |}];
  (* A plain reset restores both from cache - no recomputation. *)
  Memo.reset Memo.Invalidation.empty;
  run_both ();
  [%expect
    {|
    a 1 = 2
    b 2 = 4
    |}];
  (* clear_caches empties every registered table, forcing both to recompute. *)
  Memo.reset (Memo.Invalidation.clear_caches ~reason:Reason.Test);
  run_both ();
  [%expect
    {|
    compute a 1
    a 1 = 2
    compute b 2
    b 2 = 4
    |}]
;;

let%expect_test "changed_paths returns the deduplicated Path_changed reasons only" =
  let inv reason = Memo.Invalidation.custom ~reason ~f:(fun () -> ()) in
  let combined =
    List.fold_left
      ~init:Memo.Invalidation.empty
      ~f:Memo.Invalidation.combine
      [ inv (Reason.Path_changed (Path.of_string "b"))
      ; inv (Reason.Path_changed (Path.of_string "a"))
      ; inv (Reason.Path_changed (Path.of_string "b")) (* duplicate *)
      ; inv Reason.Test
      ; inv (Reason.Variable_changed "x")
      ]
  in
  List.iter (Memo.Invalidation.changed_paths combined) ~f:(fun path ->
    printfn "%s" (Path.to_string path));
  [%expect
    {|
    a
    b
    |}]
;;
