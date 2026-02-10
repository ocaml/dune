type t

val main : t
val at_exit : t -> (unit -> unit) -> t
val at_exit_ignore : t -> (unit -> unit) -> unit
