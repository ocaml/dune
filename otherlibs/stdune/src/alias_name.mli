type t

val compare : t -> t -> Ordering.t
val hash : t -> int
val of_string_opt_loose : string -> t option
val of_string_opt : string -> t option
val of_string : string -> t
val equal : t -> t -> bool
val to_string : t -> string
val to_dyn : t -> Dyn.t

include Comparable_intf.S with type key := t
