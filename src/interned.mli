(** Interned strings *)

open! Import

module type S = sig
  type t

  val make : string -> t
  val compare : t -> t -> Ordering.t

  module Set : sig
    include Set.S with type elt = t

    val make : string list -> t
  end

  module Map : Map.S with type key = t
end

module Make() : S
