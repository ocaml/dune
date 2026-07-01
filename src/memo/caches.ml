open! Import

(* We can get rid of this once we use the memoization system more pervasively
   and all the dependencies are properly specified *)
let cleaners = ref []
let register ~clear = cleaners := clear :: !cleaners
let clear () = List.iter !cleaners ~f:(fun f -> f ())
