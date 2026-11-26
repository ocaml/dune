open Import

val set_entry_resolver
  :  (Context_name.t -> Package.Name.t -> Install.Entry.Sourced.Unexpanded.t list Memo.t)
  -> unit

(** Env extension for an action depending on a package set. Returns an env
    with PATH, OCAMLPATH, etc. prepended for the layout root, and registers
    the action's dependency on every install entry the layout produces for
    the set. *)
val env : Context_name.t -> Package.Name.Set.t -> Env.t Action_builder.t

(** Engine dispatch for [_build/_private/<context>/.packages/<rest>]. Called
    from [Gen_rules]; the layout dir is owned by this module. Resolves to:
    - no rules for the [.packages/] root itself ([rest = []]),
    - the symlink rules for one package set when [rest = [ key ]],
      or no rules if [key] is not a known digest,
    - redirect-to-parent for any deeper path. *)
val gen_rules
  :  Context_name.t
  -> dir:Path.Build.t
  -> string list
  -> Build_config.Gen_rules.t Memo.t

module For_rocq_only : sig
  (** Do not use! Escape hatch reserved for the Rocq rule generator. See the
      implementation for details. *)

  (** DO NOT USE!!

      Returns the layout [lib] roots suitable for passing to Rocq with [-I].
      Theory packages are fully materialized, including their [rocq.d]
      entries. Plugin-only packages omit [rocq.d] entries to avoid cycles when
      a theory uses a plugin from its own package. *)
  val lib_roots
    :  Context_name.t
    -> theory_packages:Package.Name.Set.t
    -> plugin_packages:Package.Name.Set.t
    -> Path.Build.t list Action_builder.t
end
