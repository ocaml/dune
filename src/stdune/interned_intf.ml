module type S = sig
  type t

  val hash : t -> int

  val equal : t -> t -> bool

  val compare : t -> t -> Ordering.t

  val to_dyn : t -> Dyn.t

  val to_string : t -> string

  val make : string -> t

  (** Like [make] except it returns [None] if the string hasn't been registered
      with [make] previously. *)
  val get : string -> t option

  (** Return the list of all existing [t]s. *)
  val all : unit -> t list

  module Set : sig
    include Set.S with type elt = t

    val to_dyn : t -> Dyn.t

    val make : string list -> t
  end

  module Map : Map.S with type key = t

  (** Same as a hash table, but optimized for the case where we are using one
      entry for every possible [t] *)
  module Table : sig
    type key = t

    type 'a t

    val create : default_value:'a -> 'a t

    val get : 'a t -> key -> 'a

    val set : 'a t -> key:key -> data:'a -> unit
  end
  with type key := t
end
