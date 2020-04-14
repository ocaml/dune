module type S = sig
  type t

  module Set : Set_intf.S with type elt = t

  module Map : Map_intf.S with type key = t

  val gen : unit -> t
  (** Generate a new id. *)

  val peek : unit -> t
  (** Get the next id that would be generated, without actually generating it. *)

  val to_int : t -> int
  (** Convert the id to an integer. *)

  val compare : t -> t -> Ordering.t
  (** Compare two ids. *)

  val equal : t -> t -> bool

  val hash : t -> int

  val to_dyn : t -> Dyn.t
end

(** A functor to create a new ID generator module. *)
module Make () : S
