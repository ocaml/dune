(** Universal maps *)

module type Key = sig
  type 'a t
  type 'a info

  val create : 'a info -> 'a t
end

module type S = sig
  (** A universal map is a map that can store values for arbitrary keys. It is
      the the key that conveys the type of the data associated to it. *)
  type t

  module Key : sig
    type 'a t
    type 'a info
  end

  val empty : t
  val is_empty : t -> bool
  val mem : t -> 'a Key.t -> bool
  val set : t -> 'a Key.t -> 'a -> t
  val add : t -> 'a Key.t -> 'a -> (t, 'a) Result.t
  val update : t -> 'a Key.t -> f:('a option -> 'a option) -> t
  val remove : t -> 'a Key.t -> t
  val find : t -> 'a Key.t -> 'a option
  val find_exn : t -> 'a Key.t -> 'a
  val singleton : 'a Key.t -> 'a -> t

  (** [superpose a b] is [a] augmented with bindings of [b] that are not in [a]. *)
  val superpose : t -> t -> t

  type 'acc fold = { fold : 'a. 'a Key.info -> 'a -> 'acc -> 'acc }

  val fold : t -> init:'acc -> f:'acc fold -> 'acc
end
