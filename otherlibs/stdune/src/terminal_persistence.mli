type t =
  | Preserve
  | Clear_on_rebuild
  | Clear_on_rebuild_and_flush_history

val all : (string * t) list
val repr : t Repr.t
val equal : t -> t -> bool
val to_dyn : t -> Dyn.t
