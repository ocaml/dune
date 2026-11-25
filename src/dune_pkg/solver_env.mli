(** Environments in which package dependencies are solved.

    This is the dune equivalent of Opam filter environments. They contain:
    - A fixed map of variables, that is translated verbatim.
    - The settings for individual with-* qualifiers, which are translated on a
      per-package basis according to dune semantics. *)

open Import

(** Map of solver variables. *)
module Variables : sig
  type t

  val empty : t
  val equal : t -> t -> bool
  val hash : t -> int
  val repr : t Repr.t
  val compare : t -> t -> ordering
  val to_dyn : t -> Dyn.t
  val is_empty : t -> bool

  (** [is_subset t ~of_] is true iff each binding in [t] is also present in
      [of_] *)
  val is_subset : t -> of_:t -> bool

  val encode : t Encoder.t
  val decode : t Decoder.t
  val set : t -> Package_variable_name.t -> Variable_value.t -> t
  val get : t -> Package_variable_name.t -> Variable_value.t option
  val mem : t -> Package_variable_name.t -> bool

  (** [extend a b] is [a] overwritten with the content of [b]. *)
  val extend : t -> t -> t

  val pp : t -> 'a Pp.t
  val pp_oneline : t -> 'a Pp.t
  val unset_multi : t -> Package_variable_name.Set.t -> t

  (** [remove_all_except t names] is [t] with only the bindings whose names
      appear in [names]. *)
  val remove_all_except : t -> Package_variable_name.Set.t -> t

  (** [remove_all_except_platform_specific t] is [t] with only the bindings that
      are platform-specific. *)
  val remove_all_except_platform_specific : t -> t

  (** A list of environments comprising the most common platforms. Dune will
      solve dependencies for these platforms unless alternative platforms are
      specified in dune-workspace. *)
  val popular_platform_envs : t list

  (** [to_env t] is the opam filter environment binding the global variables
      of [t], and nothing else. *)
  val to_env : t -> OpamFilter.env

  (** Assign each unset platform variable in [t] to a sentinel value. For a
      description of how sentinel values are chosen and why they are necessary,
      see the documentation of [Variable_value.sentinel_value_of_variable_name]. *)
  val add_sentinel_values_for_unset_platform_vars : t -> t

  module Map : Map.S with type key = t
  module Set : Set.S with type elt = t and type 'a map = 'a Map.t
end

(** Whether and for which packages to enable dev-setup, doc and test
    qualifiers. *)
module With : sig
  (** Which packages one of these variables applies to. *)
  module Value : sig
    type t =
      | None (** Disabled globally. *)
      | Local (** Enabled only for workspace packages. *)

    val equal : t -> t -> bool
    val repr : t Repr.t
    val to_dyn : t -> Dyn.t
    val to_string : t -> string
  end

  type 'a t =
    { with_test : 'a
    ; with_doc : 'a
    ; with_dev_setup : 'a
    }

  val repr : 'a Repr.t -> 'a t Repr.t
  val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool

  (** No value set. *)
  val unset : 'a option t

  val map : 'a t -> f:('a -> 'b) -> 'b t

  (** [encode t] lists the qualifiers that are {!Value.Local} in [t]. *)
  val encode : Value.t t -> Dune_lang.t list

  val decode : Value.t t Decoder.t
end

(** An environment in which package dependencies are solved. *)
type t

val make : Variables.t -> With.Value.t With.t -> t
val equal : t -> t -> bool
val hash : t -> int
val compare : t -> t -> ordering
val to_dyn : t -> Dyn.t
val variables : t -> Variables.t
val with_ : t -> With.Value.t With.t
val pp : t -> 'a Pp.t

(** [get t name] is [Variables.get (variables t) name]. *)
val get : t -> Package_variable_name.t -> Variable_value.t option

(** [extend t variables] is [t] with its variables overwritten with
    [variables]. *)
val extend : t -> Variables.t -> t

val unset_multi : t -> Package_variable_name.Set.t -> t

(** [add_sentinel_values_for_unset_platform_vars t] is [t] with
    [Variables.add_sentinel_values_for_unset_platform_vars] applied to its
    variables. *)
val add_sentinel_values_for_unset_platform_vars : t -> t

(** [without_test t] is [t] where "with-test" applies to no package. *)
val without_test : t -> t

(** [combine ~dune_version ~current_system ~context ~with_ ~unset] is the
    combination of the default variables, [current_system] and [context] in
    increasing priority order and with variables in [unset] removed.

    The default variables are:
    - ["opam-version"], set to the version of opam vendored in Dune;
    - ["post"], set to ["true"].

    The values absent from [with_] default to:
    - {!With.Value.Local} for ["with-test"];
    - {!With.Value.None} for ["with-doc"];
    - {!With.Value.Local} for ["with-dev-setup"] when [dune_version] is 3.26 or
      later, {!With.Value.None} before. *)
val combine
  :  dune_version:Dune_lang.Syntax.Version.t
  -> current_system:Variables.t option
  -> context:Variables.t option
  -> with_:With.Value.t option With.t
  -> unset:Package_variable_name.Set.t option
  -> t

(** [to_env ~local t] is the opam filter produced by [t].

    The filter depends on whether the package is [local], ie. comes from
    workspace as opposed to the lock dir. *)
val to_env : local:bool -> t -> OpamFilter.env

module Map : Map.S with type key = t
