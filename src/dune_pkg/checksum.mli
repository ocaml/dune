open Import

(** [t] represents a checksum for a file that is to be fetched or has been
    fetched already. *)
type t

include Stringlike with type t := t

(** [to_opam_hash c] converts [c] to the representation of has values used by
    OPAM *)
val to_opam_hash : t -> OpamHash.t

(** [of_opam_hash h] converts [h] from the representation used by OPAM to Dune's *)
val of_opam_hash : OpamHash.t -> t

val pp : t -> 'a Pp.t
val equal : t -> t -> bool
