include module type of Stdlib.ArrayLabels with type 'a t = 'a array

val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
val map : 'a t -> f:('a -> 'b) -> 'b t
val exists : 'a t -> f:('a -> bool) -> bool
val fold_right : 'a t -> f:('a -> 'acc -> 'acc) -> init:'acc -> 'acc

module Immutable : sig
  type 'a t

  val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
  val get : 'a t -> int -> 'a
  val of_array : 'a array -> 'a t
  val to_list : 'a t -> 'a list
  val of_list : 'a list -> 'a t
  val map : 'a t -> f:('a -> 'b) -> 'b t
  val to_dyn : ('a -> Dyn.t) -> 'a t -> Dyn.t
  val fold_right : 'a t -> f:('a -> 'acc -> 'acc) -> init:'acc -> 'acc
  val exists : 'a t -> f:('a -> bool) -> bool
  val length : _ t -> int
  val to_list_map : 'a t -> f:('a -> 'b) -> 'b list
  val of_list_map : 'a list -> f:('a -> 'b) -> 'b t
end
