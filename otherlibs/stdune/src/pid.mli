(** Process id's as returned by the Unix family of functions *)
type t

val to_dyn : t -> Dyn.t
val hash : t -> int
val equal : t -> t -> bool
val to_int : t -> int
val me : unit -> t

(** Unsafe cast of integers to pids. Will be removed once we improve the API
    further *)
val of_int_exn : int -> t

val kill : t -> [ `Pid | `Group ] -> Signal.t -> unit

module Set : Set.S with type elt = t
