type t = int

val compare : t -> t -> Ordering.t

val equal : t -> t -> bool

val hash : t -> int

val to_dyn : t -> Dyn.t

module Set : Set.S with type elt = t

module Map : Map.S with type key = t

val of_string_exn : string -> t

val of_string : string -> t option

val to_string : t -> string

module Infix : Comparator.OPS with type t = t
