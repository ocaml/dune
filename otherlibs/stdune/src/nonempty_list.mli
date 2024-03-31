(** A (to be expanded) collection of functions for working with non-empty lists. *)

type 'a t = ( :: ) of 'a * 'a list

val hd : 'a t -> 'a
val of_list : 'a list -> 'a t option
val to_list : 'a t -> 'a list
