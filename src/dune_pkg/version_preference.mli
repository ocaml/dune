open Import

type t =
  | Newest
  | Oldest

val equal : t -> t -> bool
val to_string : t -> string
val to_dyn : t -> Dyn.t
val default : t
val all_by_string : (string * t) list
val decode : t Decoder.t
