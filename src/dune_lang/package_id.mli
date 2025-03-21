open Stdune

type t

val name : t -> Package_name.t
val dir : t -> Path.Source.t
val create : name:Package_name.t -> dir:Path.Source.t -> t
val hash : t -> int

include Comparable_intf.S with type key := t

val to_dyn : t -> Dyn.t
