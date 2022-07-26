(** OCaml sources *)

(** This module encapsulates the structure of source files in a particular
    directory. *)

open Import

module Artifacts : sig
  type t

  val lookup_module :
    t -> Module_name.t -> (Path.Build.t Obj_dir.t * Module.t) option

  val lookup_library : t -> Lib_name.t -> Lib_info.local option
end

type t

val artifacts : t -> Artifacts.t Memo.t

type for_ =
  | Library of Lib_name.t  (** Library name *)
  | Exe of
      { first_exe : string
            (** Name of first executable appearing in executables stanza *)
      }

val modules_and_obj_dir : t -> for_:for_ -> Modules.t * Path.Build.t Obj_dir.t

(** Modules attached to a library or executable.*)
val modules : t -> for_:for_ -> Modules.t

(** Find out what buildable a module is part of *)
val lookup_module : t -> Module_name.t -> Dune_file.Buildable.t option

val empty : t

(** This [lookup_vlib] argument is required for constructing the collection of
    modules for an implementation of a virtual library.

    We need to know the contents of the virtual library to: - verify conditions
    all virtual modules are implemented - make sure that we construct [Module.t]
    with the correct [kind] *)

val make :
     Dune_file.t
  -> dir:Path.Build.t
  -> scope:Scope.t
  -> lib_config:Lib_config.t
  -> loc:Loc.t
  -> lookup_vlib:(dir:Path.Build.t -> t Memo.t)
  -> include_subdirs:Loc.t * Dune_file.Include_subdirs.t
  -> dirs:(Path.Build.t * 'a list * String.Set.t) list
  -> t Memo.t
