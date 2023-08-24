include module type of Pp with type 'a t = 'a Pp.t

(** This version of [Pp.compare] uses [Ordering.t] rather than returning an [int]. *)
val compare : compare:('a -> 'a -> Ordering.t) -> 'a t -> 'a t -> Ordering.t

val to_dyn : ('a -> Dyn.t) -> 'a t -> Dyn.t

(** [truncate length pp] truncates [pp] to at most [length] characters. If the [pp]
    would go over the [length] then an ellipsis is added at the end (still within
    length). *)
val truncate : int -> 'a t -> 'a t
