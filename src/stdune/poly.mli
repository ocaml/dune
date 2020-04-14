val hash : _ -> int
(** A collection of "polymorphic" functions *)

val compare : 'a -> 'a -> Ordering.t

val equal : 'a -> 'a -> bool

val ( = ) : 'a -> 'a -> bool

val ( >= ) : 'a -> 'a -> bool

val ( > ) : 'a -> 'a -> bool

val ( <= ) : 'a -> 'a -> bool

val ( < ) : 'a -> 'a -> bool

val ( <> ) : 'a -> 'a -> bool
