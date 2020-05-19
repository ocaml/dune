(** An expander is able to expand any dune template. It has two modes of
    expansion:

    1. Static. In this mode it will only expand variables that do not introduce
    dependncies

    2. Dynamic. In this mode, the expander will record dependencies that are
    introduced by forms it has failed to expand. Later, these dependenceis can
    be filled for a full expansion.*)
open Stdune

type t

val scope : t -> Scope.t

val dir : t -> Path.Build.t

val context : t -> Context.t

val make :
     scope:Scope.t
  -> context:Context.t
  -> lib_artifacts:Artifacts.Public_libs.t
  -> bin_artifacts_host:Artifacts.Bin.t
  -> find_package:(Package.Name.t -> Package.t option)
  -> t

val set_foreign_flags :
  t -> f:(dir:Path.Build.t -> string list Build.t Foreign.Language.Dict.t) -> t

val set_env : t -> var:string -> value:string -> t

val hide_env : t -> var:string -> t

val set_dir : t -> dir:Path.Build.t -> t

val set_scope : t -> scope:Scope.t -> t

val set_bin_artifacts : t -> bin_artifacts_host:Artifacts.Bin.t -> t

val set_artifacts_dynamic : t -> bool -> t

val set_lookup_ml_sources :
  t -> f:(dir:Path.Build.t -> Ml_sources.Artifacts.t) -> t

(** Expander need to expand custom bindings sometimes. For exmaple, the name of
    the library for the action that runs inline tests. This is the place to add
    such bindings. *)
val add_bindings : t -> bindings:Pform.Map.t -> t

val extend_env : t -> env:Env.t -> t

val expand :
  t -> mode:'a String_with_vars.Mode.t -> template:String_with_vars.t -> 'a

val expand_path : t -> String_with_vars.t -> Path.t

val expand_str : t -> String_with_vars.t -> string

module Or_exn : sig
  val expand_path : t -> String_with_vars.t -> Path.t Or_exn.t

  val expand_str : t -> String_with_vars.t -> string Or_exn.t
end

type reduced_var_result =
  | Unknown
  | Restricted
  | Expanded of Value.t list

val expand_with_reduced_var_set :
  context:Context.t -> reduced_var_result String_with_vars.expander

(** Prepare a temporary expander capable of expanding variables in the [deps] or
    similar fields. This expander doesn't support variables that require us to
    build something to expand. For example, [%{exe:foo}] is allowed but
    [%{read:bar}] is not allowed.

    Once [f] has returned, the temporary expander can no longer be used. *)
val expand_deps_like_field :
  t -> dep_kind:Lib_deps_info.Kind.t -> f:(t -> 'a Build.t) -> 'a Build.t

(** Expand user actions. Both [partial] and [final] receive temporary expander
    that must not be used once these functions have returned. The expander
    passed to [partial] will not expand forms such as [%{read:...}], but the one
    passed to [final] will.

    Returns both the result of partial and final expansion. *)
val expand_action :
     t
  -> deps_written_by_user:Path.t Bindings.t Build.t
  -> targets_written_by_user:Targets.Or_forbidden.t
  -> dep_kind:Lib_deps_info.Kind.t
  -> partial:(t -> 'a)
  -> final:(t -> 'a -> 'b)
  -> 'a * 'b Build.t

(** Expand individual string templates with this function *)
val expand_var_exn : t -> Value.t list option String_with_vars.expander

(** Expand forms of the form (:standard \ foo bar). Expansion is only possible
    inside [Build.t] because such forms may contain the form (:include ..) which
    needs files to be built. *)
val expand_and_eval_set :
     t
  -> Ordered_set_lang.Unexpanded.t
  -> standard:string list Build.t
  -> string list Build.t

val eval_blang : t -> Blang.t -> bool

val map_exe : t -> Path.t -> Path.t

val artifacts : t -> Artifacts.Bin.t

val find_package : t -> Package.Name.t -> Package.t option
