open Stdune

type t = bool ref

let to_dyn _ = Dyn.opaque

let current = ref (ref true)

let restart () =
  !current := false;
  current := ref true

let current () = !current

let is_current t = !t
