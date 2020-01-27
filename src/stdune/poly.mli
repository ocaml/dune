(** A collection of "polymorphic" functions *)
val hash : _ -> int

val compare : 'a -> 'a -> Ordering.t
