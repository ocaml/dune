open Import

type t =
  | Directory of Path.t
  | Repo of Rev_store.At_rev.t

val equal : t -> t -> bool
val to_dyn : t -> Dyn.t
