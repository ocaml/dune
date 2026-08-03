type t =
  [ `Enabled
  | `Disabled
  ]

val repr : t Repr.t
val all : (string * t) list
val equal : t -> t -> bool
val of_string : string -> (t, string) result
val to_string : t -> string
val to_dyn : t -> Dyn.t
val enabled : t -> bool
val of_bool : bool -> t
