open! Stdune
module Run = Memo.Run.For_tests
module Pair = Memo.Run.For_tests.Pair

(* [Run.Pair] packs the [(last_changed_at, last_validated_at)] timestamps of a cached
   value into a single immediate int, with the invariant
   [last_changed_at <= last_validated_at]. These tests pin the round-trip. *)

let pack ~last_changed_at ~last_validated_at =
  Pair.create
    ~last_changed_at:(Run.of_int last_changed_at)
    ~last_validated_at:(Run.of_int last_validated_at)
;;

let show label p =
  Printf.printf
    "%s: last_changed_at=%d last_validated_at=%d\n"
    label
    (Run.to_int (Pair.last_changed_at p))
    (Run.to_int (Pair.last_validated_at p))
;;

let%expect_test "Run.Pair round-trips the timestamps it packs" =
  (* The pair of two invalid runs. *)
  show "invalid" Pair.invalid;
  [%expect {| invalid: last_changed_at=-1 last_validated_at=-1 |}];
  (* A grid of valid pairs: equal endpoints, a gap, the invalid lower bound, and a wide
     gap near the packing headroom. *)
  show "0,0" (pack ~last_changed_at:0 ~last_validated_at:0);
  show "0,5" (pack ~last_changed_at:0 ~last_validated_at:5);
  show "5,5" (pack ~last_changed_at:5 ~last_validated_at:5);
  show "-1,0" (pack ~last_changed_at:(-1) ~last_validated_at:0);
  show "1000,1000000" (pack ~last_changed_at:1000 ~last_validated_at:1000000);
  show "0,2^20" (pack ~last_changed_at:0 ~last_validated_at:(1 lsl 20));
  [%expect
    {|
    0,0: last_changed_at=0 last_validated_at=0
    0,5: last_changed_at=0 last_validated_at=5
    5,5: last_changed_at=5 last_validated_at=5
    -1,0: last_changed_at=-1 last_validated_at=0
    1000,1000000: last_changed_at=1000 last_validated_at=1000000
    0,2^20: last_changed_at=0 last_validated_at=1048576
    |}];
  (* [with_last_validated_at] keeps [last_changed_at] and updates the validated timestamp
     (here a bump, then a no-op). *)
  let p = pack ~last_changed_at:3 ~last_validated_at:3 in
  show "bumped" (Pair.with_last_validated_at p ~last_validated_at:(Run.of_int 9));
  show "noop" (Pair.with_last_validated_at p ~last_validated_at:(Run.of_int 3));
  [%expect
    {|
    bumped: last_changed_at=3 last_validated_at=9
    noop: last_changed_at=3 last_validated_at=3
    |}]
;;
