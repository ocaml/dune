type t = int
let compare (a : int) b : Ordering.t =
  if a < b then
    Lt
  else if a = b then
    Eq
  else
    Gt
