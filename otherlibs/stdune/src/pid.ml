type t = int

let to_dyn (t : t) : Dyn.t = Variant ("pid", [ Int t ])
let hash = Int.hash
let equal = Int.equal
let to_int t = t

let of_int_exn t =
  assert (t > 0);
  t
;;

let me () = Unix.getpid ()

let kill pid where signal =
  let where =
    match where with
    | `Pid -> pid
    | `Group ->
      assert (not Sys.win32);
      -pid
  in
  Unix.kill where (Signal.to_int signal)
;;

module Set = Int.Set
