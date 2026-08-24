(** A (to be expanded) collection of functions for working with non-empty lists. *)

type 'a t = ( :: ) of 'a * 'a list

val hd : 'a t -> 'a
val length : 'a t -> int
val iter : 'a t -> f:('a -> unit) -> unit
val for_all : 'a t -> f:('a -> bool) -> bool
val last : 'a t -> 'a
val destruct_last : 'a t -> 'a list * 'a
val rev : 'a t -> 'a t
val of_list : 'a list -> 'a t option
val of_list_exn : 'a list -> 'a t
val to_list : 'a t -> 'a list
val to_list_map : 'a t -> f:('a -> 'b) -> 'b list
val map : 'a t -> f:('a -> 'b) -> 'b t

(** Raises [Invalid_argument] if the lists have different lengths. *)
val map2 : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t

val compare : 'a t -> 'a t -> compare:('a -> 'a -> Ordering.t) -> Ordering.t
val concat : 'a list -> 'a t -> 'a t

(** same as [concat]. *)
val ( @ ) : 'a list -> 'a t -> 'a t
