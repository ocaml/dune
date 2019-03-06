type t = Dyn0.t =
  | Unit
  | Int of int
  | Bool of bool
  | String of string
  | Bytes of bytes
  | Char of char
  | Float of float
  | Option of t option
  | List of t list
  | Array of t array
  | Tuple of t list
  | Record of (string * t) list
  | Variant of string * t list
  | Map of (t * t) list
  | Set of t list

val pp : Format.formatter -> t -> unit

val opaque : t

val to_sexp : t Sexp.Encoder.t

val option : ('a -> t) -> 'a option -> t
