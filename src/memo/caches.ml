open! Import

(* We can get rid of this once we use the memoization system more pervasively
   and all the dependencies are properly specified *)
let invalidators = ref []
let register ~invalidate = invalidators := invalidate :: !invalidators
let invalidate () = List.iter !invalidators ~f:(fun f -> f ())
