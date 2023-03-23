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

module XXH3_128bits = struct
  external string : string -> string = "xxh3_128_string"
  (* external hash_64bits : string -> Int64.t = "xxh3_64_string" *)

  external file : Unix.file_descr -> string = "xxh_128_fd"

  module Stream = struct
    type t

    external create : unit -> t = "xxh_create"

    external feed_bytes : t -> Bytes.t -> pos:int -> len:int -> unit
      = "xxh_feed_bytes"

    external hash : t -> string = "xxh_128bits"
    external reset : t -> unit = "xxh_reset"
  end
end
