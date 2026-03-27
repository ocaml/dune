type t = unit

let repr = Repr.unit
let equal () () = true
let compare () () = Ordering.Eq
let to_dyn = Repr.to_dyn repr
let hash () = 0
