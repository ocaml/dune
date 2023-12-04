module type S = sig
  type 'a t
  type key

  val create : int -> 'a t
  val clear : 'a t -> unit
  val mem : 'a t -> key -> bool
  val remove : 'a t -> key -> unit
  val to_seq_values : 'a t -> 'a Seq.t
  val iter : 'a t -> f:('a -> unit) -> unit
  val set : 'a t -> key -> 'a -> unit
  val add_exn : 'a t -> key -> 'a -> unit
  val add : 'a t -> key -> 'a -> (unit, 'a) Result.t
  val find : 'a t -> key -> 'a option
  val find_exn : 'a t -> key -> 'a
  val find_or_add : 'a t -> key -> f:(key -> 'a) -> 'a
  val fold : 'a t -> init:'b -> f:('a -> 'b -> 'b) -> 'b
  val foldi : 'a t -> init:'b -> f:(key -> 'a -> 'b -> 'b) -> 'b
  val of_list_exn : (key * 'a) list -> 'a t
  val keys : _ t -> key list
  val to_dyn : ('v -> Dyn.t) -> 'v t -> Dyn.t
  val filteri_inplace : 'a t -> f:(key:key -> data:'a -> bool) -> unit
  val length : _ t -> int
end
