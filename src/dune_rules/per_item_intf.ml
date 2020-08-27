open! Dune_engine

module type S = sig
  type key

  type 'a t

  (** Create a mapping where all keys map to the same value *)
  val for_all : 'a -> 'a t

  (** Create a mapping from a list of bindings *)
  val of_mapping :
    (key list * 'a) list -> default:'a -> ('a t, key * 'a * 'a) result

  (** Get the configuration for the given item *)
  val get : 'a t -> key -> 'a

  (** Returns [true] if the mapping returns the same value for all keys. Note
      that the mapping might still be constant if [is_constant] returns [false]. *)
  val is_constant : _ t -> bool

  val map : 'a t -> f:('a -> 'b) -> 'b t

  val fold : 'a t -> init:'acc -> f:('a -> 'acc -> 'acc) -> 'acc

  val exists : 'a t -> f:('a -> bool) -> bool
end
