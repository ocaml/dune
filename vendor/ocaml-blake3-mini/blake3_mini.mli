module Digest : sig
  type t

  val hash : t -> int
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val to_binary : t -> string
  val to_hex : t -> string
  val of_hex : string -> t option
end

type t

val create : unit -> t
val reset : t -> unit
val feed_string : t -> string -> pos:int -> len:int -> unit
val feed_bytes : t -> bytes -> pos:int -> len:int -> unit

val feed_bigstring_release_lock
  :  t
  -> (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t
  -> pos:int
  -> len:int
  -> unit

val digest : t -> Digest.t
val fd : Unix.file_descr -> Digest.t
