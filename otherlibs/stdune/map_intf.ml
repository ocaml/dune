module type Key = sig
  include Comparator.S

  val to_dyn : t -> Dyn.t
end

module type S = sig
  type key

  type +'a t

  val empty : 'a t

  val is_empty : 'a t -> bool

  val mem : 'a t -> key -> bool

  val set : 'a t -> key -> 'a -> 'a t

  val add : 'a t -> key -> 'a -> ('a t, 'a) Result.t

  val add_exn : 'a t -> key -> 'a -> 'a t

  val update : 'a t -> key -> f:('a option -> 'a option) -> 'a t

  val singleton : key -> 'a -> 'a t

  val remove : 'a t -> key -> 'a t

  val add_multi : 'a list t -> key -> 'a -> 'a list t

  val merge :
    'a t -> 'b t -> f:(key -> 'a option -> 'b option -> 'c option) -> 'c t

  val union : 'a t -> 'a t -> f:(key -> 'a -> 'a -> 'a option) -> 'a t

  (** Like [union] but raises a code error if a key appears in both maps. *)
  val union_exn : 'a t -> 'a t -> 'a t

  (** [superpose a b] is [b] augmented with bindings of [a] that are not in [b]. *)
  val superpose : 'a t -> 'a t -> 'a t

  val compare : 'a t -> 'a t -> compare:('a -> 'a -> Ordering.t) -> Ordering.t

  val equal : 'a t -> 'a t -> equal:('a -> 'a -> bool) -> bool

  val iter : 'a t -> f:('a -> unit) -> unit

  val iteri : 'a t -> f:(key -> 'a -> unit) -> unit

  val iter2 : 'a t -> 'b t -> f:(key -> 'a option -> 'b option -> unit) -> unit

  val fold : 'a t -> init:'b -> f:('a -> 'b -> 'b) -> 'b

  val foldi : 'a t -> init:'b -> f:(key -> 'a -> 'b -> 'b) -> 'b

  val for_all : 'a t -> f:('a -> bool) -> bool

  val for_alli : 'a t -> f:(key -> 'a -> bool) -> bool

  val exists : 'a t -> f:('a -> bool) -> bool

  val existsi : 'a t -> f:(key -> 'a -> bool) -> bool

  val filter : 'a t -> f:('a -> bool) -> 'a t

  val filteri : 'a t -> f:(key -> 'a -> bool) -> 'a t

  val partition : 'a t -> f:('a -> bool) -> 'a t * 'a t

  val partitioni : 'a t -> f:(key -> 'a -> bool) -> 'a t * 'a t

  val cardinal : 'a t -> int

  val to_list : 'a t -> (key * 'a) list

  val to_list_map : 'a t -> f:(key -> 'a -> 'b) -> 'b list

  val of_list : (key * 'a) list -> ('a t, key * 'a * 'a) Result.t

  val of_list_map :
    'a list -> f:('a -> key * 'b) -> ('b t, key * 'a * 'a) Result.t

  val of_list_map_exn : 'a list -> f:('a -> key * 'b) -> 'b t

  val of_list_exn : (key * 'a) list -> 'a t

  val of_list_multi : (key * 'a) list -> 'a list t

  val of_list_reduce : (key * 'a) list -> f:('a -> 'a -> 'a) -> 'a t

  val of_list_reducei : (key * 'a) list -> f:(key -> 'a -> 'a -> 'a) -> 'a t

  val of_list_unit : key list -> unit t

  (** Return a map of [(k, v)] bindings such that:

      {[
        v = f init @@ f v1 @@ fv2 @@ ... @@ f vn
      ]}

      where [v1], [v2], ... [vn] are the values associated to [k] in the input
      list, in the order in which they appear. This is essentially a more
      efficient version of:

      {[
        of_list_multi l |> map ~f:(List.fold_left ~init ~f)
      ]} *)
  val of_list_fold : (key * 'a) list -> init:'b -> f:('b -> 'a -> 'b) -> 'b t

  val keys : 'a t -> key list

  val values : 'a t -> 'a list

  val min_binding : 'a t -> (key * 'a) option

  val max_binding : 'a t -> (key * 'a) option

  val choose : 'a t -> (key * 'a) option

  val split : 'a t -> key -> 'a t * 'a option * 'a t

  val find : 'a t -> key -> 'a option

  val find_exn : 'a t -> key -> 'a

  val find_key : 'a t -> f:(key -> bool) -> key option

  val map : 'a t -> f:('a -> 'b) -> 'b t

  val mapi : 'a t -> f:(key -> 'a -> 'b) -> 'b t

  val fold_mapi :
    'a t -> init:'acc -> f:(key -> 'acc -> 'a -> 'acc * 'b) -> 'acc * 'b t

  val filter_map : 'a t -> f:('a -> 'b option) -> 'b t

  val filter_mapi : 'a t -> f:(key -> 'a -> 'b option) -> 'b t

  val filter_opt : 'a option t -> 'a t

  (** [is_subset t ~of_ ~f] is [true] iff all keys in [t] are in [of_] and [f]
      is [true] for all keys that are in both. *)
  val is_subset : 'a t -> of_:'b t -> f:('a -> of_:'b -> bool) -> bool

  val to_dyn : ('a -> Dyn.t) -> 'a t -> Dyn.t

  val to_seq : 'a t -> (key * 'a) Seq.t

  module Multi : sig
    type nonrec 'a t = 'a list t

    val rev_union : 'a t -> 'a t -> 'a t

    val cons : 'a t -> key -> 'a -> 'a t

    val find : 'a t -> key -> 'a list

    val add_all : 'a t -> key -> 'a list -> 'a t

    (** [find_elt m ~f] linearly traverses the map [m] and the contained lists
        to find the first element [e] (in a list [l], mapped to key [k]) such
        that [f e = true]. If such an [e] is found then the function returns
        [Some (k,e)], otherwise it returns [None]. *)
    val find_elt : 'a t -> f:('a -> bool) -> (key * 'a) option

    val to_flat_list : 'a t -> 'a list

    val equal : 'a t -> 'a t -> equal:('a -> 'a -> bool) -> bool

    val map : 'a t -> f:('a -> 'b) -> 'b t

    val to_dyn : ('a -> Dyn.t) -> 'a t -> Dyn.t
  end
end
