type t =
  | Ignore
  | Produce

val to_dyn : t -> Dyn.t
