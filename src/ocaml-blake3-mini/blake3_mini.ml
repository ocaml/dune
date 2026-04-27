module Digest = struct
  type t = string

  let equal = String.equal
  let compare = String.compare
  let to_binary x = x

  include (
  struct
    let[@ocaml.warning "-32"] hash = Hashtbl.hash

    include String
  end :
  sig
    val hash : t -> int
  end)

  let to_hex = Digest.to_hex

  let of_hex s =
    match Digest.from_hex s with
    | s -> Some s
    | exception Invalid_argument _ -> None
  ;;
end

type t

external create : unit -> t = "blake3_mini_create"
external reset : t -> unit = "blake3_mini_reset"
external digest : t -> Digest.t = "blake3_mini_digest"

external feed_string
  :  t
  -> string
  -> pos:int
  -> len:int
  -> unit
  = "blake3_mini_feed_string"

external feed_bytes : t -> bytes -> pos:int -> len:int -> unit = "blake3_mini_feed_string"

external feed_bigstring_release_lock
  :  t
  -> (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t
  -> pos:int
  -> len:int
  -> unit
  = "blake3_mini_feed_bigstring_unlock"

external fd : Unix.file_descr -> string = "blake3_mini_fd"
