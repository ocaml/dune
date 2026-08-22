open Import

module Library : sig
  type t

  val make : package:Package.Name.t -> name:Lib_name.t -> t
  val package : t -> Package.Name.t
  val name : t -> Lib_name.t
  val to_dyn : t -> Dyn.t

  module Map : Map.S with type key = t
  module Set : Set.S with type elt = t
end

module Redirect : sig
  type t

  val make : package:Package.Name.t -> name:Lib_name.t -> t
  val package : t -> Package.Name.t
  val name : t -> Lib_name.t
  val to_dyn : t -> Dyn.t

  module Set : Set.S with type elt = t
end

type support =
  { libraries : Library.Set.t
  ; redirects : Redirect.Set.t
  ; check : unit Action_builder.t
  }

type generated_entry =
  { package : Package.Name.t
  ; section : Section.t
  ; dst : string
  ; contents : string Action_builder.t
  }

type library_entries =
  { install_entries : (Package.Name.t * Install.Entry.Sourced.Unexpanded.t) list
  ; generated_entries : generated_entry list
  }

type resolvers =
  { package_entries :
      Context_name.t -> Package.Name.t -> Install.Entry.Sourced.Unexpanded.t list Memo.t
  ; library_support : Context_name.t -> Package.Name.Set.t -> support Memo.t
  ; library_entries :
      Context_name.t -> Library.Set.t -> Redirect.Set.t -> library_entries Memo.t
  }

val set_resolvers : resolvers -> unit

(** Env extension for an action depending on a package set and its workspace
    library support closure. Returns an env with PATH, OCAMLPATH, etc. prepended
    for the layout root, and registers the action's dependency on every install
    entry the layout produces. Only selected support libraries, redirect
    metadata, and their generated metadata are included; their owning packages'
    other entries are not. *)
val env : Context_name.t -> Package.Name.Set.t -> Env.t Action_builder.t

(** Engine dispatch for [_build/install/<context>/.packages/<rest>]. Called
    from [Gen_rules]; the layout dir is owned by this module. Resolves to:
    - no rules for the [.packages/] root itself ([rest = []]),
    - the symlink and generated-file rules for the package, library, and
      redirect request encoded by [rest = [ key ]], or no rules if [key] is not
      a known digest,
    - redirect-to-parent for any deeper path. *)
val gen_rules
  :  Context_name.t
  -> dir:Path.Build.t
  -> string list
  -> Build_config.Gen_rules.result Memo.t

module For_rocq_only : sig
  (** Do not use! Escape hatch reserved for the Rocq rule generator. See the
      implementation for details. *)

  (** DO NOT USE!!

      Returns the layout's [lib] root (suitable for prepending to
      [OCAMLPATH]) and registers the action's dependency on every
      {!Section.Lib} and {!Section.Libexec} entry the layout produces for the
      set. *)
  val lib_root : Context_name.t -> Package.Name.Set.t -> Path.Build.t Action_builder.t
end
