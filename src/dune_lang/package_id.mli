open Import

type t = private
  { name : Package_name.t
  ; dir : Path.Source.t
  }

val create : name:Package_name.t -> dir:Path.Source.t -> t
val name : t -> Package_name.t
val hash : t -> int
val to_dyn : t -> Dyn.t

include Comparable_intf.S with type key := t
