type t

val length : t -> int
val set : t -> int -> bool -> unit
val create_zero : int -> t
val get : t -> int -> bool
val reset_zero : t -> unit
val pp : t Fmt.t
