include module type of struct include MoreLabels.Hashtbl end

val find : ('a, 'b) t -> 'a -> 'b option

val find_or_add : ('a, 'b) t -> 'a -> f:('a -> 'b) -> 'b
