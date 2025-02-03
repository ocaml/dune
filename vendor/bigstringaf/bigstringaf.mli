(** Bigstrings, but fast.

    The OCaml compiler has a bunch of intrinsics for Bigstrings, but they're
    not widely-known, sometimes misused, and so programs that use Bigstrings
    are slower than they have to be. And even if a library got that part right
    and exposed the intrinsics properly, the compiler doesn't have any fast blits
    between Bigstrings and other string-like types.

    So here they are. Go crazy. *)

type t =
  (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

(** {2 Constructors} *)

val create : int -> t
(** [create n] returns a bigstring of length [n] *)

val empty  : t
(** [empty] is the empty bigstring. It has length [0] and you can't really do
    much with it, but it's a good placeholder that only needs to be allocated
    once. *)

val of_string : off:int -> len:int -> string -> t
(** [of_string ~off ~len s] returns a bigstring of length [len] that contains
    the contents of string from the range [\[off, len)]. *)

val copy : t -> off:int -> len:int -> t
(** [copy t ~off ~len] allocates a new bigstring of length [len] and copies the
    bytes from [t] copied into it starting from [off]. *)

val sub : t -> off:int -> len:int -> t
(** [sub t ~off ~len] does not allocate a bigstring, but instead returns a new
    view into [t] starting at [off], and with length [len].

    {b Note} that this does not allocate a new buffer, but instead shares the
    buffer of [t] with the newly-returned bigstring. *)


(** {2 Memory-safe Operations} *)

val length : t -> int
(** [length t] is the length of the bigstring, in bytes. *)

val substring : t -> off:int -> len:int -> string
(** [substring t ~off ~len] returns a string of length [len] containing the
    bytes of [t] starting at [off]. *)

val to_string : t -> string
(** [to_string t] is equivalent to [substring t ~off:0 ~len:(length t)] *)

external get : t -> int -> char = "%caml_ba_ref_1"
(** [get t i] returns the character at offset [i] in [t]. *)

external set : t -> int -> char -> unit = "%caml_ba_set_1"
(** [set t i c] sets the character at offset [i] in [t] to be [c] *)

(** {3 Little-endian Byte Order}

    The following operations assume a little-endian byte ordering of the
    bigstring. If the machine-native byte ordering differs, then the get
    operations will reorder the bytes so that they are in machine-native byte
    order before returning the result, and the set operations will reorder the
    bytes so that they are written out in the appropriate order.

    Most modern processor architectures are little-endian, so more likely than
    not, these operations will not do any byte reordering. *)

val get_int16_le : t -> int -> int
(** [get_int16_le t i] returns the two bytes in [t] starting at offset [i],
    interpreted as an unsigned integer. *)

val get_int16_sign_extended_le : t -> int -> int
(** [get_int16_sign_extended_le t i] returns the two bytes in [t] starting at
    offset [i], interpreted as a signed integer and performing sign extension
    to the native word size before returning the result. *)

val set_int16_le : t -> int -> int -> unit
(** [set_int16_le t i v] sets the two bytes in [t] starting at offset [i] to
    the value [v]. *)

val get_int32_le : t -> int -> int32
(** [get_int32_le t i] returns the four bytes in [t] starting at offset [i]. *)

val set_int32_le : t -> int -> int32 -> unit
(** [set_int32_le t i v] sets the four bytes in [t] starting at offset [i] to
    the value [v]. *)

val get_int64_le : t -> int -> int64
(** [get_int64_le t i] returns the eight bytes in [t] starting at offset [i]. *)

val set_int64_le : t -> int -> int64 -> unit
(** [set_int64_le t i v] sets the eight bytes in [t] starting at offset [i] to
    the value [v]. *)


(** {3 Big-endian Byte Order}

    The following operations assume a big-endian byte ordering of the
    bigstring. If the machine-native byte ordering differs, then the get
    operations will reorder the bytes so that they are in machine-native byte
    order before returning the result, and the set operations will reorder the
    bytes so that they are written out in the appropriate order.

    Network byte order is big-endian, so you may need these operations when
    dealing with raw frames, for example, in a userland networking stack. *)

val get_int16_be : t -> int -> int
(** [get_int16_be t i] returns the two bytes in [t] starting at offset [i],
    interpreted as an unsigned integer. *)

val get_int16_sign_extended_be : t -> int -> int
(** [get_int16_sign_extended_be t i] returns the two bytes in [t] starting at
    offset [i], interpreted as a signed integer and performing sign extension
    to the native word size before returning the result. *)

val set_int16_be : t -> int -> int -> unit
(** [set_int16_be t i v] sets the two bytes in [t] starting at offset [off] to
    the value [v]. *)

val get_int32_be : t -> int -> int32
(** [get_int32_be t i] returns the four bytes in [t] starting at offset [i]. *)

val set_int32_be : t -> int -> int32 -> unit
(** [set_int32_be t i v] sets the four bytes in [t] starting at offset [i] to
    the value [v]. *)

val get_int64_be : t -> int -> int64
(** [get_int64_be t i] returns the eight bytes in [t] starting at offset [i]. *)

val set_int64_be : t -> int -> int64 -> unit
(** [set_int64_be t i v] sets the eight bytes in [t] starting at offset [i] to
    the value [v]. *)

(** {3 Blits}

    All the following blit operations do the same thing. They copy a given
    number of bytes from a source starting at some offset to a destination
    starting at some other offset. Forgetting for a moment that OCaml is a
    memory-safe language, these are all equivalent to:

      {[
        memcpy(dst + dst_off, src + src_off, len);
      ]}

    And in fact, that's how they're implemented. Except that bounds checking
    is performed before performing the blit. *)

val blit             : t       -> src_off:int -> t -> dst_off:int -> len:int -> unit
val blit_from_string : string  -> src_off:int -> t -> dst_off:int -> len:int -> unit
val blit_from_bytes  : Bytes.t -> src_off:int -> t -> dst_off:int -> len:int -> unit

val blit_to_bytes : t -> src_off:int -> Bytes.t -> dst_off:int -> len:int -> unit

(** {3 [memcmp]}

    Fast comparisons based on [memcmp]. Similar to the blits, these are
    implemented as C calls after performing bounds checks.

      {[
        memcmp(buf1 + off1, buf2 + off2, len);
      ]} *)

val memcmp        : t -> int -> t      -> int -> int -> int
val memcmp_string : t -> int -> string -> int -> int -> int

(** {3 [memchr]}

    Search for a byte using [memchr], returning [-1] if the byte is not found.
    Performing bounds checking before the C call. *)

val memchr : t -> int -> char -> int -> int

(** {2 Memory-unsafe Operations}

    The following operations are not memory safe. However, they do compile down
    to just a couple instructions. Make sure when using them to perform your
    own bounds checking. Or don't. Just make sure you know what you're doing.
    You can do it, but only do it if you have to. *)

external unsafe_get : t -> int -> char         = "%caml_ba_unsafe_ref_1"
(** [unsafe_get t i] is like {!get} except no bounds checking is performed. *)

external unsafe_set : t -> int -> char -> unit = "%caml_ba_unsafe_set_1"
(** [unsafe_set t i c] is like {!set} except no bounds checking is performed. *)

val unsafe_get_int16_le : t -> int -> int
(** [unsafe_get_int16_le t i] is like {!get_int16_le} except no bounds checking
    is performed. *)

val unsafe_get_int16_be : t -> int -> int
(** [unsafe_get_int16_be t i] is like {!get_int16_be} except no bounds checking
    is performed. *)

val unsafe_get_int16_sign_extended_le : t -> int -> int
(** [unsafe_get_int16_sign_extended_le t i] is like
    {!get_int16_sign_extended_le} except no bounds checking is performed. *)

val unsafe_get_int16_sign_extended_be : t -> int -> int
(** [unsafe_get_int16_sign_extended_be t i] is like
    {!get_int16_sign_extended_be} except no bounds checking is performed. *)

val unsafe_set_int16_le : t -> int -> int -> unit
(** [unsafe_set_int16_le t i v] is like {!set_int16_le} except no bounds
    checking is performed. *)

val unsafe_set_int16_be : t -> int -> int -> unit
(** [unsafe_set_int16_be t i v] is like {!set_int16_be} except no bounds
    checking is performed. *)

val unsafe_get_int32_le : t -> int -> int32
(** [unsafe_get_int32_le t i] is like {!get_int32_le} except no bounds checking
    is performed. *)

val unsafe_get_int32_be : t -> int -> int32
(** [unsafe_get_int32_be t i] is like {!get_int32_be} except no bounds checking
    is performed. *)

val unsafe_set_int32_le : t -> int -> int32 -> unit
(** [unsafe_set_int32_le t i v] is like {!set_int32_le} except no bounds
    checking is performed. *)

val unsafe_set_int32_be : t -> int -> int32 -> unit
(** [unsafe_set_int32_be t i v] is like {!set_int32_be} except no bounds
    checking is performed. *)

val unsafe_get_int64_le : t -> int -> int64
(** [unsafe_get_int64_le t i] is like {!get_int64_le} except no bounds checking
    is performed. *)

val unsafe_get_int64_be : t -> int -> int64
(** [unsafe_get_int64_be t i] is like {!get_int64_be} except no bounds checking
    is performed. *)

val unsafe_set_int64_le : t -> int -> int64 -> unit
(** [unsafe_set_int64_le t i v] is like {!set_int64_le} except no bounds
    checking is performed. *)

val unsafe_set_int64_be : t -> int -> int64 -> unit
(** [unsafe_set_int64_be t i v] is like {!set_int64_be} except no bounds
    checking is performed. *)


(** {3 Blits}

    All the following blit operations do the same thing. They copy a given
    number of bytes from a source starting at some offset to a destination
    starting at some other offset. Forgetting for a moment that OCaml is a
    memory-safe language, these are all equivalent to:

      {[
        memcpy(dst + dst_off, src + src_off, len);
      ]}

    And in fact, that's how they're implemented. Except in the case of
    [unsafe_blit] which uses a [memmove] so that overlapping blits behave as
    expected. But in both cases, there's no bounds checking. *)

val unsafe_blit             : t       -> src_off:int -> t -> dst_off:int -> len:int -> unit
val unsafe_blit_from_string : string  -> src_off:int -> t -> dst_off:int -> len:int -> unit
val unsafe_blit_from_bytes  : Bytes.t -> src_off:int -> t -> dst_off:int -> len:int -> unit

val unsafe_blit_to_bytes : t -> src_off:int -> Bytes.t -> dst_off:int -> len:int -> unit

(** {3 [memcmp]}

    Fast comparisons based on [memcmp]. Similar to the blits, these are not
    memory safe and are implemented by the same C call:

      {[
        memcmp(buf1 + off1, buf2 + off2, len);
      ]} *)

val unsafe_memcmp        : t -> int -> t      -> int -> int -> int
val unsafe_memcmp_string : t -> int -> string -> int -> int -> int

(** {3 [memchr]}

    Search for a byte using [memchr], returning [-1] if the byte is not found.
    It does not check bounds before the C call. *)

val unsafe_memchr : t -> int -> char -> int -> int
