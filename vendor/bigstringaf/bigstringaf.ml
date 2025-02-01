type bigstring =
  (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

type t = bigstring

let create size = Bigarray.(Array1.create char c_layout size)
let empty       = create 0

module BA1 = Bigarray.Array1

let length t = BA1.dim t

external get : t -> int -> char = "%caml_ba_ref_1"
external set : t -> int -> char -> unit = "%caml_ba_set_1"

external unsafe_get : t -> int -> char         = "%caml_ba_unsafe_ref_1"
external unsafe_set : t -> int -> char -> unit = "%caml_ba_unsafe_set_1"

external unsafe_blit            : t       -> src_off:int -> t       -> dst_off:int -> len:int -> unit =
  "bigstringaf_blit_to_bigstring" [@@noalloc]

external unsafe_blit_to_bytes   : t       -> src_off:int -> Bytes.t -> dst_off:int -> len:int -> unit =
  "bigstringaf_blit_to_bytes"     [@@noalloc]

external unsafe_blit_from_bytes : Bytes.t -> src_off:int -> t       -> dst_off:int -> len:int -> unit =
  "bigstringaf_blit_from_bytes"   [@@noalloc]

external unsafe_blit_from_string : string -> src_off:int -> t       -> dst_off:int -> len:int -> unit =
  "bigstringaf_blit_from_bytes"   [@@noalloc]

external unsafe_memcmp : t -> int -> t -> int -> int -> int =
  "bigstringaf_memcmp_bigstring" [@@noalloc]

external unsafe_memcmp_string : t -> int -> string -> int -> int -> int =
  "bigstringaf_memcmp_string" [@@noalloc]

external unsafe_memchr : t -> int -> char -> int -> int =
  "bigstringaf_memchr" [@@noalloc]

let sub t ~off ~len =
  BA1.sub t off len

let[@inline never] invalid_bounds op buffer_len off len =
  let message =
    Printf.sprintf "Bigstringaf.%s invalid range: { buffer_len: %d, off: %d, len: %d }"
    op buffer_len off len
  in
  raise (Invalid_argument message)
;;

let[@inline never] invalid_bounds_blit op src_len src_off dst_len dst_off len =
  let message =
    Printf.sprintf "Bigstringaf.%s invalid range: { src_len: %d, src_off: %d, dst_len: %d, dst_off: %d, len: %d }"
    op src_len src_off dst_len dst_off len
  in
  raise (Invalid_argument message)
;;

let[@inline never] invalid_bounds_memcmp op buf1_len buf1_off buf2_len buf2_off len =
  let message =
    Printf.sprintf "Bigstringaf.%s invalid range: { buf1_len: %d, buf1_off: %d, buf2_len: %d, buf2_off: %d, len: %d }"
    op buf1_len buf1_off buf2_len buf2_off len
  in
  raise (Invalid_argument message)
;;

(* A note on bounds checking.
 *
 * The code should perform the following check to ensure that the blit doesn't
 * run off the end of the input buffer:
 *
 *   {[off + len <= buffer_len]}
 *
 * However, this may lead to an integer overflow for large values of [off],
 * e.g., [max_int], which will cause the comparison to return [true] when it
 * should really return [false].
 *
 * An equivalent comparison that does not run into this integer overflow
 * problem is:
 *
 *   {[buffer_len - off => len]}
 *
 * This is checking that the input buffer, less the offset, is sufficiently
 * long to perform the blit. Since the expression is subtracting [off] rather
 * than adding it, it doesn't suffer from the overflow that the previous
 * inequality did. As long as there is a check to ensure that [off] is not
 * negative, it won't underflow either. *)

let copy t ~off ~len =
  let buffer_len = length t in
  if len < 0 || off < 0 || buffer_len - off < len
  then invalid_bounds "copy" buffer_len off len;
  let dst = create len in
  unsafe_blit t ~src_off:off dst ~dst_off:0 ~len;
  dst
;;

let substring t ~off ~len =
  let buffer_len = length t in
  if len < 0 || off < 0 || buffer_len - off < len
  then invalid_bounds "substring" buffer_len off len;
  let b = Bytes.create len in
  unsafe_blit_to_bytes t ~src_off:off b ~dst_off:0 ~len;
  Bytes.unsafe_to_string b
;;

let to_string t =
  let len = length t in
  let b = Bytes.create len in
  unsafe_blit_to_bytes t ~src_off:0 b ~dst_off:0 ~len;
  Bytes.unsafe_to_string b
;;

let of_string ~off ~len s =
  let buffer_len = String.length s in
  if len < 0 || off < 0 || buffer_len - off < len
  then invalid_bounds "of_string" buffer_len off len;
  let b = create len in
  unsafe_blit_from_string s ~src_off:off b ~dst_off:0 ~len;
  b
;;

let blit src ~src_off dst ~dst_off ~len =
  let src_len = length src in
  let dst_len = length dst in
  if len < 0
  then invalid_bounds_blit "blit" src_len src_off dst_len dst_off len;
  if src_off < 0 || src_len - src_off < len
  then invalid_bounds_blit "blit" src_len src_off dst_len dst_off len;
  if dst_off < 0 || dst_len - dst_off < len
  then invalid_bounds_blit "blit" src_len src_off dst_len dst_off len;
  unsafe_blit src ~src_off dst ~dst_off ~len
;;

let blit_from_string src ~src_off dst ~dst_off ~len =
  let src_len = String.length src in
  let dst_len = length dst in
  if len < 0
  then invalid_bounds_blit "blit_from_string" src_len src_off dst_len dst_off len;
  if src_off < 0 || src_len - src_off < len
  then invalid_bounds_blit "blit_from_string" src_len src_off dst_len dst_off len;
  if dst_off < 0 || dst_len - dst_off < len
  then invalid_bounds_blit "blit_from_string" src_len src_off dst_len dst_off len;
  unsafe_blit_from_string src ~src_off dst ~dst_off ~len
;;

let blit_from_bytes src ~src_off dst ~dst_off ~len =
  let src_len = Bytes.length src in
  let dst_len = length dst in
  if len < 0
  then invalid_bounds_blit "blit_from_bytes" src_len src_off dst_len dst_off len;
  if src_off < 0 || src_len - src_off < len
  then invalid_bounds_blit "blit_from_bytes" src_len src_off dst_len dst_off len;
  if dst_off < 0 || dst_len - dst_off < len
  then invalid_bounds_blit "blit_from_bytes" src_len src_off dst_len dst_off len;
  unsafe_blit_from_bytes src ~src_off dst ~dst_off ~len
;;

let blit_to_bytes src ~src_off dst ~dst_off ~len =
  let src_len = length src in
  let dst_len = Bytes.length dst in
  if len < 0
  then invalid_bounds_blit "blit_to_bytes" src_len src_off dst_len dst_off len;
  if src_off < 0 || src_len - src_off < len
  then invalid_bounds_blit "blit_to_bytes" src_len src_off dst_len dst_off len;
  if dst_off < 0 || dst_len - dst_off < len
  then invalid_bounds_blit "blit_to_bytes" src_len src_off dst_len dst_off len;
  unsafe_blit_to_bytes src ~src_off dst ~dst_off ~len
;;

let memcmp buf1 buf1_off buf2 buf2_off len =
  let buf1_len = length buf1 in
  let buf2_len = length buf2 in
  if len < 0
  then invalid_bounds_memcmp "memcmp" buf1_len buf1_off buf2_len buf2_off len;
  if buf1_off < 0 || buf1_len - buf1_off < len
  then invalid_bounds_memcmp "memcmp" buf1_len buf1_off buf2_len buf2_off len;
  if buf2_off < 0 || buf2_len - buf2_off < len
  then invalid_bounds_memcmp "memcmp" buf1_len buf1_off buf2_len buf2_off len;
  unsafe_memcmp buf1 buf1_off buf2 buf2_off len
;;

let memcmp_string buf1 buf1_off buf2 buf2_off len =
  let buf1_len = length buf1 in
  let buf2_len = String.length buf2 in
  if len < 0
  then invalid_bounds_memcmp "memcmp_string" buf1_len buf1_off buf2_len buf2_off len;
  if buf1_off < 0 || buf1_len - buf1_off < len
  then invalid_bounds_memcmp "memcmp_string" buf1_len buf1_off buf2_len buf2_off len;
  if buf2_off < 0 || buf2_len - buf2_off < len
  then invalid_bounds_memcmp "memcmp_string" buf1_len buf1_off buf2_len buf2_off len;
  unsafe_memcmp_string buf1 buf1_off buf2 buf2_off len
;;

let memchr buf buf_off chr len =
  let buf_len = length buf in
  if len < 0
  then invalid_bounds "memchr" buf_len buf_off len;
  if buf_off < 0 || buf_len - buf_off < len
  then invalid_bounds "memchr" buf_len buf_off len;
  unsafe_memchr buf buf_off chr len

(* Safe operations *)

external caml_bigstring_set_16 : bigstring -> int -> int   -> unit = "%caml_bigstring_set16"
external caml_bigstring_set_32 : bigstring -> int -> int32 -> unit = "%caml_bigstring_set32"
external caml_bigstring_set_64 : bigstring -> int -> int64 -> unit = "%caml_bigstring_set64"

external caml_bigstring_get_16 : bigstring -> int -> int   = "%caml_bigstring_get16"
external caml_bigstring_get_32 : bigstring -> int -> int32 = "%caml_bigstring_get32"
external caml_bigstring_get_64 : bigstring -> int -> int64 = "%caml_bigstring_get64"

module Swap = struct
  external bswap16 : int -> int = "%bswap16"
  external bswap_int32 : int32 -> int32 = "%bswap_int32"
  external bswap_int64 : int64 -> int64 = "%bswap_int64"

  let caml_bigstring_set_16 bs off i =
    caml_bigstring_set_16 bs off (bswap16 i)

  let caml_bigstring_set_32 bs off i =
    caml_bigstring_set_32 bs off (bswap_int32 i)

  let caml_bigstring_set_64 bs off i =
    caml_bigstring_set_64 bs off (bswap_int64 i)

  let caml_bigstring_get_16 bs off =
    bswap16 (caml_bigstring_get_16 bs off)

  let caml_bigstring_get_32 bs off =
    bswap_int32 (caml_bigstring_get_32 bs off)

  let caml_bigstring_get_64 bs off =
    bswap_int64 (caml_bigstring_get_64 bs off)

  let get_int16_sign_extended x off =
    ((caml_bigstring_get_16 x off) lsl (Sys.int_size - 16)) asr (Sys.int_size - 16)
end

let set_int16_le, set_int16_be =
  if Sys.big_endian
  then Swap.caml_bigstring_set_16, caml_bigstring_set_16
  else caml_bigstring_set_16     , Swap.caml_bigstring_set_16

let set_int32_le, set_int32_be =
  if Sys.big_endian
  then Swap.caml_bigstring_set_32, caml_bigstring_set_32
  else caml_bigstring_set_32     , Swap.caml_bigstring_set_32

let set_int64_le, set_int64_be =
  if Sys.big_endian
  then Swap.caml_bigstring_set_64, caml_bigstring_set_64
  else caml_bigstring_set_64     , Swap.caml_bigstring_set_64

let get_int16_le, get_int16_be =
  if Sys.big_endian
  then Swap.caml_bigstring_get_16, caml_bigstring_get_16
  else caml_bigstring_get_16     , Swap.caml_bigstring_get_16

let get_int16_sign_extended_noswap x off =
  ((caml_bigstring_get_16      x off) lsl (Sys.int_size - 16)) asr (Sys.int_size - 16)

let get_int16_sign_extended_le, get_int16_sign_extended_be  =
  if Sys.big_endian
  then Swap.get_int16_sign_extended  , get_int16_sign_extended_noswap
  else get_int16_sign_extended_noswap, Swap.get_int16_sign_extended

let get_int32_le, get_int32_be =
  if Sys.big_endian
  then Swap.caml_bigstring_get_32, caml_bigstring_get_32
  else caml_bigstring_get_32     , Swap.caml_bigstring_get_32

let get_int64_le, get_int64_be =
  if Sys.big_endian
  then Swap.caml_bigstring_get_64, caml_bigstring_get_64
  else caml_bigstring_get_64     , Swap.caml_bigstring_get_64

(* Unsafe operations *)

external caml_bigstring_unsafe_set_16 : bigstring -> int -> int   -> unit = "%caml_bigstring_set16u"
external caml_bigstring_unsafe_set_32 : bigstring -> int -> int32 -> unit = "%caml_bigstring_set32u"
external caml_bigstring_unsafe_set_64 : bigstring -> int -> int64 -> unit = "%caml_bigstring_set64u"

external caml_bigstring_unsafe_get_16 : bigstring -> int -> int   = "%caml_bigstring_get16u"
external caml_bigstring_unsafe_get_32 : bigstring -> int -> int32 = "%caml_bigstring_get32u"
external caml_bigstring_unsafe_get_64 : bigstring -> int -> int64 = "%caml_bigstring_get64u"

module USwap = struct
  external bswap16 : int -> int = "%bswap16"
  external bswap_int32 : int32 -> int32 = "%bswap_int32"
  external bswap_int64 : int64 -> int64 = "%bswap_int64"

  let caml_bigstring_unsafe_set_16 bs off i =
    caml_bigstring_unsafe_set_16 bs off (bswap16 i)

  let caml_bigstring_unsafe_set_32 bs off i =
    caml_bigstring_unsafe_set_32 bs off (bswap_int32 i)

  let caml_bigstring_unsafe_set_64 bs off i =
    caml_bigstring_unsafe_set_64 bs off (bswap_int64 i)

  let caml_bigstring_unsafe_get_16 bs off =
    bswap16 (caml_bigstring_unsafe_get_16 bs off)

  let caml_bigstring_unsafe_get_32 bs off =
    bswap_int32 (caml_bigstring_unsafe_get_32 bs off)

  let caml_bigstring_unsafe_get_64 bs off =
    bswap_int64 (caml_bigstring_unsafe_get_64 bs off)
end

let unsafe_set_int16_le, unsafe_set_int16_be =
  if Sys.big_endian
  then USwap.caml_bigstring_unsafe_set_16, caml_bigstring_unsafe_set_16
  else caml_bigstring_unsafe_set_16      , USwap.caml_bigstring_unsafe_set_16

let unsafe_set_int32_le, unsafe_set_int32_be =
  if Sys.big_endian
  then USwap.caml_bigstring_unsafe_set_32, caml_bigstring_unsafe_set_32
  else caml_bigstring_unsafe_set_32      , USwap.caml_bigstring_unsafe_set_32

let unsafe_set_int64_le, unsafe_set_int64_be =
  if Sys.big_endian
  then USwap.caml_bigstring_unsafe_set_64, caml_bigstring_unsafe_set_64
  else caml_bigstring_unsafe_set_64      , USwap.caml_bigstring_unsafe_set_64

let unsafe_get_int16_le, unsafe_get_int16_be =
  if Sys.big_endian
  then USwap.caml_bigstring_unsafe_get_16, caml_bigstring_unsafe_get_16
  else caml_bigstring_unsafe_get_16      , USwap.caml_bigstring_unsafe_get_16

let unsafe_get_int16_sign_extended_le x off =
  ((unsafe_get_int16_le x off) lsl (Sys.int_size - 16)) asr (Sys.int_size - 16)

let unsafe_get_int16_sign_extended_be x off =
  ((unsafe_get_int16_be x off ) lsl (Sys.int_size - 16)) asr (Sys.int_size - 16)

let unsafe_get_int32_le, unsafe_get_int32_be =
  if Sys.big_endian
  then USwap.caml_bigstring_unsafe_get_32, caml_bigstring_unsafe_get_32
  else caml_bigstring_unsafe_get_32      , USwap.caml_bigstring_unsafe_get_32

let unsafe_get_int64_le, unsafe_get_int64_be =
  if Sys.big_endian
  then USwap.caml_bigstring_unsafe_get_64, caml_bigstring_unsafe_get_64
  else caml_bigstring_unsafe_get_64      , USwap.caml_bigstring_unsafe_get_64
