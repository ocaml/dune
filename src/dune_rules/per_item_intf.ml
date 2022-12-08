open Import

module type S = sig
  type key

  type 'a t

  val to_dyn : ('a -> Dyn.t) -> 'a t -> Dyn.t

  val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool

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

  val fold_resolve :
       'a t
    -> init:'acc
    -> f:('a -> 'acc -> 'acc Resolve.Memo.t)
    -> 'acc Resolve.Memo.t

  val exists : 'a t -> f:('a -> bool) -> bool

  val map_action_builder :
    'a t -> f:('a -> 'b Action_builder.t) -> 'b t Action_builder.t

  val map_resolve : 'a t -> f:('a -> 'b Resolve.Memo.t) -> 'b t Resolve.Memo.t
end
