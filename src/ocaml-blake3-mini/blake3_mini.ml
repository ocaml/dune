module Digest = struct
  (* Float-only record fields are stored unboxed. The fields contain int64 bits
     rather than floating-point values. *)
  type t =
    { first : float
    ; second : float
    }

  external equal : t -> t -> bool = "blake3_mini_digest_equal" [@@noalloc]
  external compare : t -> t -> int = "blake3_mini_digest_compare" [@@noalloc]

  let hash { first; _ } = Int64.to_int (Int64.bits_of_float first)

  let to_binary { first; second } =
    let result = Bytes.create 16 in
    Bytes.set_int64_ne result 0 (Int64.bits_of_float first);
    Bytes.set_int64_ne result 8 (Int64.bits_of_float second);
    Bytes.unsafe_to_string result
  ;;

  let to_hex digest = Digest.to_hex (to_binary digest)

  let of_hex s =
    match Digest.from_hex s with
    | s ->
      Some
        { first = Int64.float_of_bits (String.get_int64_ne s 0)
        ; second = Int64.float_of_bits (String.get_int64_ne s 8)
        }
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

external fd : Unix.file_descr -> Digest.t = "blake3_mini_fd"
external file_with_size_unix : string -> Digest.t * int = "blake3_mini_file_with_size"

let file_with_size_ocaml file =
  let digest_fd = fd in
  let fd = Unix.openfile file [ Unix.O_RDONLY; Unix.O_SHARE_DELETE; Unix.O_CLOEXEC ] 0 in
  match
    let size = (Unix.fstat fd).st_size in
    let digest = digest_fd fd in
    digest, size
  with
  | exception exn ->
    let bt = Printexc.get_raw_backtrace () in
    (match Unix.close fd with
     | () -> ()
     | exception _ -> ());
    Printexc.raise_with_backtrace exn bt
  | res ->
    Unix.close fd;
    res
;;

let file_with_size = if Sys.win32 then file_with_size_ocaml else file_with_size_unix
