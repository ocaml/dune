type t =
  [ `Int of int
  | `Float of float
  | `String of string
  | `List of t list
  | `Bool of bool
  | `Assoc of (string * t) list
  | `Null
  ]

val to_string : t -> string
val string : string -> t
val assoc : (string * t) list -> t
val list : t list -> t
val int : int -> t
val float : float -> t
val bool : bool -> t
