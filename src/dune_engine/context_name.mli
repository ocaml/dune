open Import

type t

val is_default : t -> bool
val build_dir : t -> Path.Build.t
val default : t
val hash : t -> int
val target : t -> toolchain:t -> t
val equal : t -> t -> bool
val compare : t -> t -> Ordering.t

include Stringlike with type t := t
module Infix : Comparator.OPS with type t = t
include Comparable_intf.S with type key := t
module Top_closure : Top_closure with type key := t and type 'a monad := 'a Monad.Id.t
