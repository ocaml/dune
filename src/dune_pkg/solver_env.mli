open Import

type t

val empty : t
val equal : t -> t -> bool
val hash : t -> int
val repr : t Repr.t
val compare : t -> t -> ordering
val to_dyn : t -> Dyn.t
val is_empty : t -> bool

(** [is_subset t ~of_] is true iff each binding in [t] is also present in [of_] *)
val is_subset : t -> of_:t -> bool

val encode : t Encoder.t
val decode : t Decoder.t
val decode_workspace : t Decoder.t
val set : t -> Package_variable_name.t -> Variable_value.t -> t
val get : t -> Package_variable_name.t -> Variable_value.t option

(** [extend a b] adds all variables from [b] to [a] overwriting any
    existing values of those variables in [a]. *)
val extend : t -> t -> t

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

module Scope : sig
  type t =
    | Workspace
    | Dependencies

  val of_package
    :  local_packages:'a Dune_lang.Package_name.Map.t
    -> Dune_lang.Package_name.t
    -> t
end

(** Resolved environments for the two fixed package scopes. *)
module Scoped : sig
  type env := t
  type t

  (** Resolve scope defaults, system values, shared assignments, shared unsets,
      then workspace assignments. Without workspace assignments, preserve the
      legacy shared environment. *)
  val resolve
    :  current_system:env option
    -> shared:env option
    -> workspace:env option
    -> unset:Package_variable_name.Set.t option
    -> t

  val is_scoped : t -> bool
  val get : t -> Scope.t -> env

  (** Apply shared platform bindings before explicit workspace assignments. *)
  val for_platform : t -> Scope.t -> env -> env

  val encode : t -> platforms:env list -> Dune_lang.t
  val remove_platform_specific : t -> t
  val add_sentinel_values_for_unset_platform_vars : t -> t
end
