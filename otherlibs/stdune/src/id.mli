module type S = sig
  type t [@@immediate]

  include Comparable_intf.S with type key := t
  module Table : Hashtbl.S with type key = t

  module Hashset : sig
    type id := t
    type t

    val create : unit -> t
    val is_empty : t -> bool
    val add : t -> id -> unit
    val mem : t -> id -> bool
    val to_dyn : t -> Dyn.t
  end

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
