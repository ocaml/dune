open Import

val set_entry_resolver
  :  (Context_name.t -> Package.Name.t -> Install.Entry.Sourced.Unexpanded.t list Memo.t)
  -> unit

(** Env extension for an action depending on a package set. Returns an env
    with PATH, OCAMLPATH, etc. prepended for the layout root, and registers
    the action's dependency on every install entry the layout produces for
    the set. *)
val env : Context_name.t -> Package.Name.Set.t -> Env.t Action_builder.t

(** Engine dispatch for [_build/install/<context>/.packages/<rest>]. Called
    from [Gen_rules]; the layout dir is owned by this module. Resolves to:
    - no rules for the [.packages/] root itself ([rest = []]),
    - the symlink rules for one package set when [rest = [ key ]],
      or no rules if [key] is not a known digest,
    - redirect-to-parent for any deeper path. *)
val gen_rules
  :  Context_name.t
  -> dir:Path.Build.t
  -> string list
  -> Build_config.Gen_rules.result Memo.t

module For_rocq_only : sig
  (** Do not use! Escape hatch reserved for the Rocq rule generator. See the
      implementation for details. *)

  (** Returns the layout's [lib] root, suitable for prepending to
      [OCAMLPATH], without recording dependencies. *)
  val lib_root : Context_name.t -> Package.Name.Set.t -> Path.Build.t

  (** Registers dependencies on every {!Section.Lib} and {!Section.Libexec}
      entry. Files in all-library directories are grouped to share dependency
      hashes; mixed-directory files remain explicit dependencies. Both are
      materialized in sandboxes. *)
  val deps : Context_name.t -> Package.Name.Set.t -> unit Action_builder.t
end
