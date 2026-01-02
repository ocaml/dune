(** A (to be expanded) collection of functions for working with non-empty lists. *)

type 'a t = ( :: ) of 'a * 'a list

val hd : 'a t -> 'a
val last : 'a t -> 'a
val rev : 'a t -> 'a t
val of_list : 'a list -> 'a t option
val of_list_exn : 'a list -> 'a t
val to_list : 'a t -> 'a list
val to_list_map : 'a t -> f:('a -> 'b) -> 'b list
val map : 'a t -> f:('a -> 'b) -> 'b t
val compare : 'a t -> 'a t -> compare:('a -> 'a -> Ordering.t) -> Ordering.t
val concat : 'a list -> 'a t -> 'a t

(** same as [concat]. *)
val ( @ ) : 'a list -> 'a t -> 'a t
