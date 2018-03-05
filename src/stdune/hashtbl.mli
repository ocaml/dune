include module type of struct include MoreLabels.Hashtbl end

val add : ('a, 'b) t -> 'a -> 'b -> unit

val find : ('a, 'b) t -> 'a -> 'b option
val find_or_add : ('a, 'b) t -> 'a -> f:('a -> 'b) -> 'b

val fold  : ('a, 'b) t -> init:'c -> f:(      'b -> 'c -> 'c) -> 'c
val foldi : ('a, 'b) t -> init:'c -> f:('a -> 'b -> 'c -> 'c) -> 'c
