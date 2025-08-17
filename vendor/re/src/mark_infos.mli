(** store mark information for groups in an array *)
type t

val make : (int * int) list -> t
val offset : t -> int -> (int * int) option
val test : t -> int -> bool
val iteri : t -> f:(int -> int -> int -> unit) -> unit
