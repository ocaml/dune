(** Interned strings *)

open! Import

module type S = sig
  type t
  val compare : t -> t -> Ordering.t
  val to_string : t -> string

  val make : string -> t

  (** Like [make] except it returns [None] if the string hasn't been
      registered with [make] previously. *)
  val get : string -> t option

  module Set : sig
    include Set.S with type elt = t

    val make : string list -> t
  end

  module Map : Map.S with type key = t

  (** Same as a hash table, but optimized for the case where we are
      using one entry for every possible [t] *)
  module Table : sig
    type key = t
    type 'a t

    val create : unit -> 'a t

    val get : 'a t -> key -> 'a option
    val set : 'a t -> key:key -> data:'a -> unit
  end with type key := t
end

module Make() : S
