type t = float

let repr = Repr.float

let of_string f =
  try Some (float_of_string f) with
  | _ -> None
;;

let to_string = string_of_float
let equal = Stdlib.Float.equal
let compare x y = Ordering.of_int (compare x y)
let to_dyn = Repr.to_dyn repr

let max x y =
  match compare x y with
  | Eq | Gt -> x
  | Lt -> y
;;
