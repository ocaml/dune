module T = struct
  type t = int

  let compare (a : int) b : Ordering.t =
    if a < b then Lt else if a = b then Eq else Gt

  let to_dyn x = Dyn.Int x
end

include T
include Comparable.Make (T)

let equal (a : t) b = a = b

let hash (t : t) = t

let of_string_exn s =
  match int_of_string s with
  | exception Failure _ ->
    failwith (Printf.sprintf "of_string_exn: invalid int %S" s)
  | s -> s

let to_string i = string_of_int i

module Infix = Comparator.Operators (T)

let of_string s = int_of_string_opt s

let shift_left = Stdlib.Int.shift_left

let shift_right = Stdlib.Int.shift_right
