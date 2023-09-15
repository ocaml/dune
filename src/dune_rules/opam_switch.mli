type t =
  { root : string option
  ; switch : string
  }

val equal : t -> t -> bool
val hash : t -> int
val to_dyn : t -> Dyn.t
