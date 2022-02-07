open Stdune

type t = int

let compare = Int.compare
let to_dyn = Dyn.int
let current = ref 0
let is_current t = Int.equal !current t
let restart () = incr current
let current () = !current
