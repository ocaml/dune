include module type of Stdlib.ArrayLabels with type 'a t = 'a array

val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool

module Immutable : sig
  type 'a t

  val of_array : 'a array -> 'a t

  val to_list : 'a t -> 'a list

  val of_list : 'a list -> 'a t

  val map : 'a t -> f:('a -> 'b) -> 'b t
end
