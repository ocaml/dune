open Import

module type S = sig
  type 'a map
  type el
  type key = el Nonempty_list.t

  type 'a t = 'a node map

  and 'a node =
    | Leaf of 'a
    | Map of 'a t

  val empty : 'a t
  val map : 'a t -> f:('a -> 'b) -> 'b t
  val mapi : 'a t -> f:(key -> 'a -> 'b) -> 'b t
  val of_map : 'a map -> 'a t
  val find : 'a t -> key -> 'a option
  val set : 'a t -> key -> 'a -> 'a t
  val set_map : 'a t -> el list -> 'a map -> ('a t, 'a node) result
  val remove : 'a t -> key -> 'a t
  val mem : 'a t -> key -> bool
  val fold : 'a t -> init:'acc -> f:('a -> 'acc -> 'acc) -> 'acc
  val foldi : 'a t -> init:'acc -> f:(key -> 'a -> 'acc -> 'acc) -> 'acc
  val to_dyn : ('a -> Dyn.t) -> 'a t -> Dyn.t
  val to_map : 'a t -> 'a map
  val to_list : 'a t -> 'a list
  val to_list_map : 'a t -> f:('a -> 'b) -> 'b list
  val to_list_mapi : 'a t -> f:(key -> 'a -> 'b) -> 'b list
  val singleton : key -> 'a -> 'a t
  val merge : 'a t -> 'b t -> f:(key -> 'a option -> 'b option -> 'c option) -> 'c t
  val as_singleton : 'a t -> 'a option
  val filter_map : 'a t -> f:('a -> 'b option) -> 'b t
  val toplevel_only : 'a t -> 'a map
end
