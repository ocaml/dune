type t = string

module D = Dune_caml.Digest

module Set = String.Set

let file p = D.file (Path.to_string p)

let compare x y = Ordering.of_int (D.compare x y)

let to_hex = D.to_hex

let from_hex = D.from_hex

let string = D.string

let to_string s = s
