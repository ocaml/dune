open Stdune
open Dune_util

type t

val compare : t -> t -> Ordering.t

val equal : t -> t -> bool

val hash : t -> int

include Comparable_intf.S with type key := t

include Dune_sexp.Conv.S with type t := t

include Stringlike with type t := t
