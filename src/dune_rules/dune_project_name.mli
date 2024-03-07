open Import

(** Invariants: - Named s -> s <> "" and s does not contain '.' or '/' -
    Anonymous p -> p is a local path in the source tree *)
type t

val to_dyn : t -> Dyn.t
val equal : t -> t -> bool
val compare : t -> t -> Ordering.t

(** Convert to a string that is suitable for human readable messages *)
val to_string_hum : t -> string

module Infix : Comparator.OPS with type t = t
module Map : Map.S with type key = t

val anonymous : Path.Source.t -> t
val named : Loc.t -> string -> t
val encode : t Dune_sexp.Encoder.t
val decode : t Dune_sexp.Decoder.t
val name : t -> string option
