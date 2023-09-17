open Import

(** Represents a valid external lib name *)
type t

include Stringlike with type t := t

val equal : t -> t -> bool
val compare : t -> t -> Ordering.t
val to_module_name : t -> Module_name.t
val clean : t -> t
