open! Stdune
open! Memo.O
open Test_helpers.Make ()
module Reason = Memo.Invalidation.Reason

(* [Lazy.Expert.create] returns both the underlying cell and the forcing thunk; they read
   the same memoized value, computed once. *)
let%expect_test "Lazy.Expert.create exposes the cell and the thunk" =
  let calls = ref 0 in
  let cell, thunk =
    Memo.Lazy.Expert.create ~name:"expert" (fun () ->
      incr calls;
      printfn "computing";
      Memo.return 42)
  in
  printfn "thunk = %d" (run (Memo.Lazy.force thunk));
  printfn "cell = %d" (run (Memo.Cell.read cell));
  printfn "calls = %d" !calls;
  [%expect
    {|
    computing
    thunk = 42
    cell = 42
    calls = 1
    |}];
  (* Invalidating the cell forces recomputation. *)
  Memo.reset (Memo.Cell.invalidate ~reason:Reason.Test cell);
  printfn "thunk = %d" (run (Memo.Lazy.force thunk));
  printfn "calls = %d" !calls;
  [%expect
    {|
    computing
    thunk = 42
    calls = 2
    |}]
;;
