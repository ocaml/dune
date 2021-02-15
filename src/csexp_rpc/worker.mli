(** Simple queue that is consumed by its own thread *)

type 'work t

val create : spawn:((unit -> unit) -> unit) -> ('a -> unit) -> 'a t

val add_work : 'a t -> 'a -> (unit, [ `Stopped ]) result

val stop : _ t -> unit
