module String = Dune_caml.String

include StringLabels

let drop s n =
  let len = length s in
  sub s ~pos:(min n len) ~len:(max (len - n) 0)
