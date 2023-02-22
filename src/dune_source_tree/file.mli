open Stdune

type t =
  { ino : int
  ; dev : int
  }

val dummy : t

val to_dyn : t -> Dyn.t

val compare : t -> t -> Ordering.t

module Map : Map.S with type key = t

module Make (Reduced_stats : Reduced_stats_intf.S) : sig
  val of_stats : Reduced_stats.t -> t
end
