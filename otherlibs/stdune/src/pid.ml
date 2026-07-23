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

let signal pid where signal =
  let where =
    match where with
    | `Pid -> pid
    | `Group ->
      assert (not Sys.win32);
      -pid
  in
  match
    Unix.kill
      where
      (match signal with
       | `Null -> 0
       | `Signal s -> Signal.to_int s)
  with
  | () -> `Delivered
  | exception Unix.Unix_error (Unix.EPERM, _, _) -> `Not_allowed
  | exception Unix.Unix_error (Unix.ESRCH, _, _) -> `Dead
;;

let check pid where =
  match signal pid where `Null with
  | `Not_allowed | `Delivered -> `Alive
  | `Dead -> `Dead
;;

let kill pid where (signal' : Signal.t) =
  match signal pid where (`Signal signal') with
  | `Delivered -> `Delivered
  | `Dead -> `Dead
  | `Not_allowed ->
    Code_error.raise
      "Not allowed to signal"
      [ "signal", Signal.to_dyn signal'; "pid", to_dyn pid ]
;;

let kill_exn pid where signal =
  match kill pid where signal with
  | `Delivered -> ()
  | `Dead ->
    Code_error.raise "Pid is dead" [ "signal", Signal.to_dyn signal; "pid", to_dyn pid ]
;;

module Set = Int.Set
