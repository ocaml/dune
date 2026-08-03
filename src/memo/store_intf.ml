module type Input = sig
  type t

  val to_dyn : t -> Dyn.t
end

module type S = sig
  type key
  type 'a t

  val create : unit -> 'a t
  val clear : 'a t -> unit
  val set : 'a t -> key -> 'a -> unit
  val find : 'a t -> key -> 'a option

  (** [find_or_add t key ~f] returns the value associated with [key], or, if [key] is not
      present, adds [f key] and returns it, in a single hash-table lookup. *)
  val find_or_add : 'a t -> key -> f:(key -> 'a) -> 'a

  val iter : 'a t -> f:('a -> unit) -> unit
end

module type Instance = sig
  type key
  type value
  type t

  val clear : t -> unit
  val set : t -> key -> value -> unit
  val find : t -> key -> value option

  (** [find_or_add t key ~f] returns the value associated with [key], or, if [key] is not
      present, adds [f key] and returns it. This performs a single hash-table lookup,
      unlike a [find] followed by a [set]. *)
  val find_or_add : t -> key -> f:(key -> value) -> value

  val iter : t -> f:(value -> unit) -> unit
  val store : t
end
