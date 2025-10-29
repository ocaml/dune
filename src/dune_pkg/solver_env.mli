open Import

type t

val empty : t
val equal : t -> t -> bool
val hash : t -> int
val digest_feed : t Dune_digest.Feed.t
val compare : t -> t -> ordering
val to_dyn : t -> Dyn.t
val is_empty : t -> bool

(** [is_subset t ~of_] is true iff each binding in [t] is also present in [of_] *)
val is_subset : t -> of_:t -> bool

val encode : t Encoder.t
val decode : t Decoder.t
val set : t -> Package_variable_name.t -> Variable_value.t -> t
val get : t -> Package_variable_name.t -> Variable_value.t option

(** [extend a b] adds all variables from [b] to [a] overwriting any
    existing values of those variables in [a]. *)
val extend : t -> t -> t

(** A [t] with default values for some variables:
    - "with-doc" is set to "false"
    - "with-dev-setup" is set to "false"
    - "opam-version" is set to the version of opam vendored in dune *)
val with_defaults : t

val pp : t -> 'a Pp.t
val pp_oneline : t -> 'a Pp.t
val unset_multi : t -> Package_variable_name.Set.t -> t

(** [remove_all_except t names] returns an environment with the same bindings
    as those in [t] whose names also appear in [names]. *)
val remove_all_except : t -> Package_variable_name.Set.t -> t

(** Remove all bindings from the environment except for those which are
    platform-specific. *)
val remove_all_except_platform_specific : t -> t

val to_env : t -> OpamFilter.env

(** A list of environments comprising the most common platforms. Dune will
    solve dependencies for these platforms unless alternative platforms are
    specified in dune-workspace. *)
val popular_platform_envs : t list

(** Assign each unset platform variable in [t] to a sentinel value. For a
    description of how sentinel values are chosen and why they are necessary,
    see the documentation of [Variable_value.sentinel_value_of_variable_name]. *)
val add_sentinel_values_for_unset_platform_vars : t -> t

module Map : Map.S with type key = t
module Set : Set.S with type elt = t and type 'a map = 'a Map.t
