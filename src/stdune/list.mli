type 'a t = 'a list

include module type of struct include ListLabels end

val is_empty : _ t -> bool

val filter_map : 'a t -> f:('a -> 'b option) -> 'b t

val filteri : 'a t -> f:(int -> 'a -> bool) -> 'a t

val concat_map : 'a t -> f:('a -> 'b t) -> 'b t

val partition_map : 'a t -> f:('a -> ('b, 'c) Either.t) -> 'b t * 'c t

val find     : 'a t -> f:('a -> bool     ) -> 'a option
val find_map : 'a t -> f:('a -> 'b option) -> 'b option

val max_length : 'a t -> length:('a -> int) -> int

val last : 'a t -> 'a option
