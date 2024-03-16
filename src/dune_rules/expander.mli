(** An expander is able to expand any dune template. *)

open Import

type t

val dir : t -> Path.Build.t
val context : t -> Context_name.t
val project : t -> Dune_project.t

val make_root
  :  project:Dune_project.t
  -> scope:Scope.t Memo.t
  -> scope_host:Scope.t Memo.t
  -> context:Context.t
  -> env:Env.t Memo.t
  -> public_libs:Lib.DB.t Memo.t
  -> public_libs_host:Lib.DB.t Memo.t
  -> artifacts_host:Artifacts.t Memo.t
  -> t

val set_local_env_var : t -> var:string -> value:string Action_builder.t -> t

val set_scope
  :  t
  -> dir:Path.Build.t
  -> project:Dune_project.t
  -> scope:Scope.t Memo.t
  -> scope_host:Scope.t Memo.t
  -> t

val set_artifacts : t -> artifacts_host:Artifacts.t Memo.t -> t

module Expanding_what : sig
  type t =
    | Nothing_special
    | Deps_like_field
    | User_action of Path.Build.t Targets_spec.t
    | User_action_without_targets of { what : string }
    (** [what] describe what the action is. It should be a plural and is
        inserted in a sentence as follow: "<what> are not allowed to have
        targets" *)
end

(** Used to improve error messages and handing special cases, such as:
    [%{exe:fn}] maps [fn] to the host context except when expanding a deps-like
    field. *)
val set_expanding_what : t -> Expanding_what.t -> t

(** Expander needs to expand custom bindings sometimes. For example, the name of
    the library for the action that runs inline tests. This is the place to add
    such bindings. *)
val add_bindings : t -> bindings:Value.t list Pform.Map.t -> t

module Deps : sig
  type 'a t =
    | Without of 'a Memo.t
    | With of 'a Action_builder.t

  include Applicative with type 'a t := 'a t

  val action_builder : 'a t -> 'a Action_builder.t
end

type value = Value.t list Deps.t

val add_bindings_full : t -> bindings:value Pform.Map.t -> t
val extend_env : t -> env:Env.t Memo.t -> t

val expand
  :  t
  -> mode:('deferred, 'value) String_with_vars.Mode.t
  -> String_with_vars.t
  -> 'value Action_builder.t

val expand_path : t -> String_with_vars.t -> Path.t Action_builder.t
val expand_str : t -> String_with_vars.t -> string Action_builder.t
val expand_pform : t -> Value.t list Action_builder.t String_with_vars.expander
val expand_str_partial : t -> String_with_vars.t -> String_with_vars.t Action_builder.t

module No_deps : sig
  (** Same as [expand_xxx] but disallow percent forms that introduce action
      dependencies, such as [%{dep:...}] *)

  val expand_pform : t -> Value.t list Memo.t String_with_vars.expander

  val expand
    :  t
    -> mode:('deferred, 'value) String_with_vars.Mode.t
    -> String_with_vars.t
    -> 'value Memo.t

  val expand_path : t -> String_with_vars.t -> Path.t Memo.t
  val expand_str : t -> String_with_vars.t -> string Memo.t
end

module With_deps_if_necessary : sig
  (** Same as [expand_xxx] but stay in the [Memo] monad if possible. *)

  val expand_path : t -> String_with_vars.t -> Path.t list Deps.t
end

val expand_ordered_set_lang
  :  t
  -> Ordered_set_lang.Unexpanded.t
  -> Ordered_set_lang.t Action_builder.t

(** Expand forms of the form (:standard \ foo bar). Expansion is only possible
    inside [Action_builder.t] because such forms may contain the form (:include
    ..) which needs files to be built. *)
val expand_and_eval_set
  :  t
  -> Ordered_set_lang.Unexpanded.t
  -> standard:string list Action_builder.t
  -> string list Action_builder.t

val eval_blang : t -> Blang.t -> bool Memo.t
val map_exe : t -> Path.t -> Path.t
val artifacts : t -> Artifacts.t Memo.t
val expand_locks : t -> Locks.t -> Path.t list Action_builder.t

val foreign_flags
  : (dir:Path.Build.t -> string list Action_builder.t Foreign_language.Dict.t Memo.t)
      Fdecl.t

val lookup_artifacts : (dir:Path.Build.t -> Artifacts_obj.t Memo.t) Fdecl.t
val to_expander0 : t -> Expander0.t
