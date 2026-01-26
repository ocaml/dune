(** A speciallized buffer module used to emit traces in a single [write].

  This is not a general purpose module.

  Callers are responsible to make sure the buffer is big enough before writing
  anything to it. *)

type t

val create : int -> t
val add_char : t -> char -> unit
val add_string : t -> string -> unit
val buf : t -> Bigstringaf.t
val pos : t -> int
val clear : t -> unit
val to_string : t -> string
val drop : t -> int -> unit
val available : t -> int
val max_size : t -> int
val resize : t -> int -> unit
