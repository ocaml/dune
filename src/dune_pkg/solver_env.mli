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

(** A [t] with default values for some variables:
    - "with-doc" is set to "false"
    - "opam-version" is set to the version of opam vendored in dune *)
val with_defaults : t

val pp : t -> 'a Pp.t
val unset_multi : t -> Variable_name.Set.t -> t
val to_env : t -> OpamFilter.env
