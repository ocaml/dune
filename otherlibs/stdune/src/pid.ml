type t = int

let to_dyn (t : t) : Dyn.t = Variant ("pid", [ Int t ])
let hash = Int.hash
let equal = Int.equal
let to_int t = t

let of_int t =
  assert (t >= 0);
  t
;;
