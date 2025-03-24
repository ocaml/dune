(* added in OCaml 5.2 *)
let[@warning "-32"] pp_infinity = Int.max_int

include Stdlib.Format
