module type S = sig
  module Set : sig
    type elt
    type t

    val empty : t
    val singleton : elt -> t
    val choose : t -> elt option
    val iter : t -> f:(elt -> unit) -> unit
    val to_dyn : t -> Dyn.t
    val is_empty : t -> bool
    val mem : t -> elt -> bool
    val of_list : elt list -> t
    val of_sorted_list : elt list -> t
    val union : t -> t -> t
    val equal : t -> t -> bool
    val is_subset : t -> of_:t -> bool
    val are_disjoint : t -> t -> bool
    val diff : t -> t -> t
    val filter : t -> f:(elt -> bool) -> t
    val fold : t -> init:'a -> f:(elt -> 'a -> 'a) -> 'a
    val to_list : t -> elt list
    val to_list_map : t -> f:(elt -> 'a) -> 'a list
  end

  module Map : sig
    type key = Set.elt
    type 'a t

    val empty : 'a t
    val is_empty : 'a t -> bool
    val choose : 'a t -> (key * 'a) option
    val choose_key : 'a t -> key option
    val mem : 'a t -> key -> bool
    val find : 'a t -> key -> 'a option
    val of_list_exn : (key * 'a) list -> 'a t
    val of_sorted_list_exn : (key * 'a) list -> 'a t
    val union_left_biased : 'a t -> 'a t -> 'a t
    val keys : _ t -> Set.t
    val of_set : Set.t -> f:(key -> 'a) -> 'a t
    val values : 'a t -> 'a list
    val exists : 'a t -> f:('a -> bool) -> bool
    val foldi : 'a t -> init:'acc -> f:(key -> 'a -> 'acc -> 'acc) -> 'acc
    val filter_mapi : 'a t -> f:(key -> 'a -> 'b option) -> 'b t
    val mapi : 'a t -> f:(key -> 'a -> 'b) -> 'b t
    val to_seq : 'a t -> (key * 'a) Seq.t
    val to_list : 'a t -> (key * 'a) list
    val to_list_map : 'a t -> f:(key -> 'a -> 'b) -> 'b list
    val to_dyn : ('a -> Dyn.t) -> 'a t -> Dyn.t
    val equal : 'a t -> 'a t -> equal:('a -> 'a -> bool) -> bool
    val iteri : 'a t -> f:(key -> 'a -> unit) -> unit
  end
end
