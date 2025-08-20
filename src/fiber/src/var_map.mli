(** A heterogeneous total map data structure optimized for small collections. *)
type t

module Key : sig
  (** A key to a map. *)
  type 'a t

  (** Create a new key for use in a map. This function is expensive and should be called
      sparingly: the map is optimized for a small number of keys created at startup.

      The input value will be used as the initial value of the key in the map. You can
      pick ['a = 'b option] and pass [None] if the key has no reasonable initial value. *)
  val create : 'a -> 'a t
end

(** The map with no entries. *)
val empty : t

(** Update or insert an entry. *)
val set : t -> 'a Key.t -> 'a -> t

(** Retrieve the value stored at a key. Returns the initial value if the key has never
    been [set]. *)
val get : t -> 'a Key.t -> 'a

(** [update t key f] is a shortcut for [set t key (f (get t key))]. *)
val update : t -> 'a Key.t -> f:('a -> 'a) -> t
