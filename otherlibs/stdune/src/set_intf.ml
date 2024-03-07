module type S = sig
  type elt
  type t
  type 'a map

  val empty : t
  val is_empty : t -> bool
  val mem : t -> elt -> bool
  val add : t -> elt -> t
  val singleton : elt -> t
  val remove : t -> elt -> t
  val union : t -> t -> t
  val inter : t -> t -> t
  val diff : t -> t -> t
  val compare : t -> t -> Ordering.t
  val equal : t -> t -> bool
  val is_subset : t -> of_:t -> bool
  val are_disjoint : t -> t -> bool
  val iter : t -> f:(elt -> unit) -> unit
  val map : t -> f:(elt -> elt) -> t
  val fold : t -> init:'a -> f:(elt -> 'a -> 'a) -> 'a
  val for_all : t -> f:(elt -> bool) -> bool
  val exists : t -> f:(elt -> bool) -> bool
  val filter : t -> f:(elt -> bool) -> t
  val partition : t -> f:(elt -> bool) -> t * t
  val cardinal : t -> int
  val min_elt : t -> elt option
  val max_elt : t -> elt option
  val choose : t -> elt option
  val choose_exn : t -> elt
  val split : t -> elt -> t * bool * t
  val of_list : elt list -> t
  val of_list_map : 'a list -> f:('a -> elt) -> t
  val to_list : t -> elt list
  val to_seq : t -> elt Seq.t

  (** Convert to a list and map every element. *)
  val to_list_map : t -> f:(elt -> 'a) -> 'a list

  val find : t -> f:(elt -> bool) -> elt option
  val union_all : t list -> t
  val union_map : 'a list -> f:('a -> t) -> t
  val to_dyn : t -> Dyn.t
  val of_keys : _ map -> t
  val to_map : t -> f:(elt -> 'a) -> 'a map
end
