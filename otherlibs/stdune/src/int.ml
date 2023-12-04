module T = struct
  type t = int

  let compare (a : int) b : Ordering.t = if a < b then Lt else if a = b then Eq else Gt
  let to_dyn x = Dyn.Int x
end

include T
include Comparable.Make (T)

let equal (a : t) b = a = b

(* This implementation (including the comment) is taken from the Base
   library. *)

(* This hash was chosen from here: https://gist.github.com/badboy/6267743

   It attempts to fulfill the primary goals of a non-cryptographic hash function:

   - a bit change in the input should change ~1/2 of the output bits
   - the output should be uniformly distributed across the output range
   - inputs that are close to each other shouldn't lead to outputs that are close to
     each other.
   - all bits of the input are used in generating the output

   In our case we also want it to be fast, non-allocating, and inlinable. *)
let[@inline always] hash (t : t) =
  let t = lnot t + (t lsl 21) in
  let t = t lxor (t lsr 24) in
  let t = t + (t lsl 3) + (t lsl 8) in
  let t = t lxor (t lsr 14) in
  let t = t + (t lsl 2) + (t lsl 4) in
  let t = t lxor (t lsr 28) in
  t + (t lsl 31)
;;

let of_string_exn s =
  match int_of_string s with
  | exception Failure _ -> failwith (Printf.sprintf "of_string_exn: invalid int %S" s)
  | s -> s
;;

let to_string i = string_of_int i

module Infix = Comparator.Operators (T)

let of_string s = int_of_string_opt s
let shift_left = Stdlib.Int.shift_left
let shift_right = Stdlib.Int.shift_right
