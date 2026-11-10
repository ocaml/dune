open Import

val set_entry_resolver
  :  (Context_name.t -> Package.Name.t -> Install.Entry.Sourced.Unexpanded.t list Memo.t)
  -> unit

type t

(** Materialize outputs and register dependencies on installed files, without
    applying exports. *)
val materialize : Context_name.t -> Package.Name.Set.t -> t Action_builder.t

val dependencies : t -> Opam_build.Dependency.t list

(** Prepend the layout's standard paths to [base], then apply package exports
    in order and record their environment dependencies. *)
val env : t -> base:Env.t -> Env.t Action_builder.t

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

      Returns the layout's [lib] root (suitable for prepending to
      [OCAMLPATH]) and registers the action's dependency on every
      {!Section.Lib} and {!Section.Libexec} entry the layout produces for the
      set. Files in all-library directories are grouped to share dependency
      hashes; mixed-directory files remain explicit dependencies. Both are
      materialized in sandboxes. *)
  val lib_root : Context_name.t -> Package.Name.Set.t -> Path.Build.t Action_builder.t
end
