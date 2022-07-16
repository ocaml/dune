(** Defines the name of sub systems. Has the same limitations has library names *)

open Import

type t

val hash : t -> int

val equal : t -> t -> bool

include Comparable_intf.S with type key := t

include Stringlike_intf.S with type t := t
