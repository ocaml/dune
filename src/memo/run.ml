open Stdune

type t = int

let compare = Int.compare
let to_dyn = Dyn.int
let current = ref 0
let is_current t = Int.equal !current t
let restart () = incr current
let current () = !current
let invalid = -1

module Pair = struct
  (* The high [63 - delta_bits] bits store [last_validated_at + 1] (so that a pair of two
     invalid runs is [0]). The low [delta_bits] bits store
     [last_validated_at - last_changed_at], which is non-negative by invariant.

     We use 31 bits for the delta and the remaining 32 bits for the validated timestamp,
     giving each plenty of headroom while still fitting in a 63-bit OCaml int. This assumes
     a 64-bit platform; the packing is not supported on 32-bit. *)
  type t = int

  let delta_bits = 31
  let delta_mask = (1 lsl delta_bits) - 1

  let[@inline always] create ~last_changed_at ~last_validated_at =
    assert (last_changed_at <= last_validated_at);
    ((last_validated_at + 1) lsl delta_bits) lor (last_validated_at - last_changed_at)
  ;;

  let[@inline always] last_validated_at t = (t lsr delta_bits) - 1
  let[@inline always] last_changed_at t = last_validated_at t - (t land delta_mask)

  let[@inline always] with_last_validated_at t ~last_validated_at =
    create ~last_changed_at:(last_changed_at t) ~last_validated_at
  ;;

  (* The pair of two invalid runs; packs to [0]. *)
  let invalid = create ~last_changed_at:invalid ~last_validated_at:invalid
end

module For_testing = struct
  let of_int t = t
  let to_int t = t
end
