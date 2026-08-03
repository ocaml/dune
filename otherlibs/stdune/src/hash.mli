(** A non-allocating hash accumulator.

    Threads an [int] seed through a sequence of input ints with a
    multiplicative combiner; designed for use by structural [hash]
    functions on records, tuples, and lists that previously built a
    temporary value to pass to [Stdlib.Hashtbl.hash].

    Hash values produced by this module are stable within a single
    process run but make no cross-process or cross-version guarantee;
    use only as a [Hashtbl] / [Memo] bucket selector. *)

type t

val create : unit -> t
val feed : t -> int -> t
val hash : t -> int
