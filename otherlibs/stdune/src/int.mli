type t = int

val repr : t Repr.t
val compare : t -> t -> Ordering.t
val equal : t -> t -> bool
val hash : t -> int
val to_dyn : t -> Dyn.t
val max : t -> t -> t

include Comparable_intf.S with type key := t

val of_string_exn : string -> t
val of_string : string -> t option
val to_string : t -> string

module Infix : Comparator.OPS with type t = t

val shift_left : t -> t -> t
val shift_right : t -> t -> t
val max_int : t
