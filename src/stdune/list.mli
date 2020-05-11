include module type of struct
  include ListLabels
end

(* ocaml/ocaml#1892 "Allow shadowing of items coming from an include" helps
   making this work in 4.08, as OCaml now includes a `List.t` type. *)
type 'a t = 'a list

val is_empty : _ t -> bool

val is_non_empty : _ t -> bool

val filter_map : 'a t -> f:('a -> 'b option) -> 'b t

val filter_opt : 'a option t -> 'a t

val filteri : 'a t -> f:(int -> 'a -> bool) -> 'a t

val concat_map : 'a t -> f:('a -> 'b t) -> 'b t

val partition_map : 'a t -> f:('a -> ('b, 'c) Either.t) -> 'b t * 'c t

val rev_map_append : 'a t -> 'b t -> f:('a -> 'b) -> 'b t

val rev_partition_map : 'a t -> f:('a -> ('b, 'c) Either.t) -> 'b t * 'c t

type ('a, 'b) skip_or_either =
  | Skip
  | Left of 'a
  | Right of 'b

val filter_partition_map :
  'a t -> f:('a -> ('b, 'c) skip_or_either) -> 'b t * 'c t

val rev_filter_partition_map :
  'a t -> f:('a -> ('b, 'c) skip_or_either) -> 'b t * 'c t

val find : 'a t -> f:('a -> bool) -> 'a option

val findi : 'a t -> f:('a -> bool) -> ('a * int) option

val find_exn : 'a t -> f:('a -> bool) -> 'a

val find_map : 'a t -> f:('a -> 'b option) -> 'b option

val last : 'a t -> 'a option

val destruct_last : 'a t -> ('a list * 'a) option

val sort : 'a t -> compare:('a -> 'a -> Ordering.t) -> 'a t

val stable_sort : 'a t -> compare:('a -> 'a -> Ordering.t) -> 'a t

val sort_uniq : 'a t -> compare:('a -> 'a -> Ordering.t) -> 'a t

val compare : 'a t -> 'a t -> compare:('a -> 'a -> Ordering.t) -> Ordering.t

val assoc : ('a * 'b) t -> 'a -> 'b option

val singleton : 'a -> 'a t

val nth : 'a t -> int -> 'a option

val physically_equal : 'a t -> 'a t -> bool

val init : int -> f:(int -> 'a) -> 'a list

val hd_opt : 'a t -> 'a option

val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool

val hash : ('a -> int) -> 'a list -> int

val cons : 'a -> 'a t -> 'a t

val fold_map : 'a list -> init:'b -> f:('b -> 'a -> 'b * 'c) -> 'b * 'c list

val unzip : ('a * 'b) t -> 'a t * 'b t

val for_all2 :
     'a list
  -> 'b list
  -> f:('a -> 'b -> bool)
  -> (bool, [ `Length_mismatch ]) result

val reduce : 'a list -> f:('a -> 'a -> 'a) -> 'a option

val min : 'a list -> f:('a -> 'a -> Ordering.t) -> 'a option

val max : 'a list -> f:('a -> 'a -> Ordering.t) -> 'a option
