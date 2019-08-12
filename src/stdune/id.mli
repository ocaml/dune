module type S = sig
  type t

  module Set : Set_intf.S with type elt = t

  module Map : Map_intf.S with type key = t

  (** Generate a new id. *)
  val gen : unit -> t

  (** Get the next id that would be generated, without actually generating it. *)
  val peek : unit -> t

  (** Convert the id to an integer. *)
  val to_int : t -> int

  (** Compare two ids. *)
  val compare : t -> t -> Ordering.t

  val equal : t -> t -> bool

  val hash : t -> int

  val to_dyn : t -> Dyn.t
end

(** A functor to create a new ID generator module. *)
module Make () : S
