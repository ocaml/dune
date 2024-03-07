type 'a t

val create : unit -> 'a t
val push : 'a t -> 'a -> unit
val pop : 'a t -> 'a option
val pop_exn : 'a t -> 'a
val peek : 'a t -> 'a option
val peek_exn : 'a t -> 'a
val clear : 'a t -> unit
val copy : 'a t -> 'a t
val is_empty : 'a t -> bool
val length : 'a t -> int
val iter : 'a t -> f:('a -> unit) -> unit
val fold : 'a t -> f:('b -> 'a -> 'b) -> init:'b -> 'b
val transfer : 'a t -> 'a t -> unit
val to_list : 'a t -> 'a list
