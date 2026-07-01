open! Stdune
open! Memo.O
open Test_helpers.Make ()
module Reason = Memo.Invalidation.Reason

(* A hand-rolled Memo store over [int] keys, backed by a mutable assoc list, to exercise
   [create_with_store]'s custom-store path (the default [create] goes through
   [Stdune.Table]). We use [Stdlib.List] explicitly for the assoc operations since
   [open Stdune] shadows [List]. *)
module Int_store = struct
  type key = int
  type 'a t = (int * 'a) list ref

  let create () = ref []
  let clear t = t := []
  let find t key = Stdlib.List.assoc_opt key !t
  let set t key v = t := (key, v) :: Stdlib.List.remove_assoc key !t

  let find_or_add t key ~f =
    match find t key with
    | Some v -> v
    | None ->
      let v = f key in
      set t key v;
      v
  ;;

  let iter t ~f = Stdlib.List.iter (fun (_, v) -> f v) !t
end

module Int_input = struct
  type t = int

  let to_dyn = Dyn.int
end

let%expect_test "create_with_store routes memoization through a custom store" =
  let calls = ref 0 in
  let f =
    Memo.create_with_store
      "custom-store"
      ~store:(module Int_store)
      ~input:(module Int_input)
      ~cutoff:Int.equal
      (fun x ->
         incr calls;
         printfn "computing %d" x;
         Memo.return (x * 10))
  in
  (* Bind the result before reading [calls] so the counter reflects the compute. *)
  let show i =
    let r = run_memo f i in
    printfn "f %d = %d (calls = %d)" i r !calls
  in
  (* First evaluation computes (store: set); a second in the same run hits the store
     (find); a distinct key computes once more. *)
  show 1;
  show 1;
  show 2;
  [%expect
    {|
    computing 1
    f 1 = 10 (calls = 1)
    f 1 = 10 (calls = 1)
    computing 2
    f 2 = 20 (calls = 2)
    |}];
  (* clear_caches empties the custom store (Store.clear), forcing a recompute. *)
  Memo.reset (Memo.Invalidation.clear_caches ~reason:Reason.Test);
  show 1;
  [%expect
    {|
    computing 1
    f 1 = 10 (calls = 3)
    |}]
;;
