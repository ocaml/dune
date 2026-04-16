type t = Unix.file_descr

let equal = Poly.equal
let hash = Poly.hash
let to_dyn = Dyn.opaque
