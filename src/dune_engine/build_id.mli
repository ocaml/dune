open Import

(** Unique identifier for a build execution. Allows tracking multiple
    concurrent builds and associating progress, errors, and hooks with
    specific build instances. *)

type t

(** Create a new unique build ID. Thread-safe via sequential counter. *)
val create : unit -> t

val equal : t -> t -> bool
val compare : t -> t -> Ordering.t
val to_dyn : t -> Dyn.t
val hash : t -> int

module Map : Map.S with type key = t
module Set : Set.S with type elt = t
