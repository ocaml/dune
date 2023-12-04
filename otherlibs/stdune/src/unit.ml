type t = unit

let equal () () = true
let compare () () = Ordering.Eq
let hash () = 0
let to_dyn () = Dyn.Unit
