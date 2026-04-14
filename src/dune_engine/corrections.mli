open Import

type t =
  | Ignore
  | Produce

val repr : t Repr.t
val to_dyn : t -> Dyn.t
