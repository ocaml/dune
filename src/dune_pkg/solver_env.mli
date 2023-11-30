open Import

type t

val empty : t
val equal : t -> t -> bool
val to_dyn : t -> Dyn.t
val is_empty : t -> bool
val decode : t Decoder.t
val set : t -> Variable_name.t -> Variable_value.t -> t
val get : t -> Variable_name.t -> Variable_value.t option

(** [extend a b] adds all variables from [b] to [a] overwriting any
    existing values of those variables in [a]. *)
val extend : t -> t -> t

(** A [t] with a single variable "opam-version" set to the version of the opam
    library dependen on by dune *)
val with_opam_version_set_to_current : t

val pp : t -> 'a Pp.t
