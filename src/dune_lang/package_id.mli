open Import

type t = private
  { name : Package_name.t
  ; dir : Source_path.t
  }

val create : name:Package_name.t -> dir:Source_path.t -> t
val name : t -> Package_name.t
val hash : t -> int
val to_dyn : t -> Dyn.t
val compare : t -> t -> Ordering.t

include Comparable_intf.S with type key := t
