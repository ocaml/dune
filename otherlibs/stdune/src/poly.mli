(** A collection of "polymorphic" functions *)
val hash : _ -> int

val compare : 'a -> 'a -> Ordering.t
val equal : 'a -> 'a -> bool
val ( = ) : 'a -> 'a -> bool
val ( >= ) : 'a -> 'a -> bool
val ( > ) : 'a -> 'a -> bool
val ( <= ) : 'a -> 'a -> bool
val ( < ) : 'a -> 'a -> bool
val ( <> ) : 'a -> 'a -> bool
