(** Bindings to the xxHash non-cryptographic hash algorithm

    https://cyan4973.github.io/xxHash/
 *)

module type S = sig
  type hash

  val string : string -> hash
  val file : Unix.file_descr -> string

  module Stream : sig
    type t

    val create : unit -> t
    val feed_bytes : t -> Bytes.t -> pos:int -> len:int -> unit
    val hash : t -> hash
    val reset : t -> unit
  end
end

module XXH3_128bits : S with type hash := string
(* TODO *)
(* module XXH3_64bit : S with type hash := int64 *)
(* module XXH64 : S with type hash := int64 *)
