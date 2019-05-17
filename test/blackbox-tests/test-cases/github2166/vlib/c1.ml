type t = unit

let one = ()

let to_string (() : t) = Printf.sprintf "%s unit" V1.foo
