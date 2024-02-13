open Stdune
include module type of Dune_section with type t = Dune_section.t

val compare : t -> t -> Ordering.t

include Comparable_intf.S with type key := t

val enum_decoder : (string * t) list
val all : Set.t
val parse_string : string -> (t, string) Result.t
val decode : t Dune_sexp.Decoder.t
val encode : t Dune_sexp.Encoder.t
val to_dyn : t -> Dyn.t

(** [true] iff the executable bit should be set for files installed in this
    location. *)
val should_set_executable_bit : t -> bool
