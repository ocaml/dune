(** OCaml sources *)

(** This module encapsulates the structure of source files in a particular
    directory. *)

open Import

module Origin : sig
  type t =
    | Library of Library.t
    | Executables of Executables.t
    | Melange of Melange_stanzas.Emit.t

  val preprocess : t -> Preprocess.With_instrumentation.t Preprocess.Per_module.t
  val loc : t -> Loc.t
  val to_dyn : t -> Dyn.t
end

type t

val artifacts : t -> Artifacts_obj.t Memo.t

type for_ =
  | Library of Lib_id.Local.t
  | Exe of
      { first_exe : string (** Name of first executable appearing in executables stanza *)
      }
  | Melange of { target : string }

val modules_and_obj_dir
  :  t
  -> libs:Lib.DB.t
  -> for_:for_
  -> (Modules.t * Path.Build.t Obj_dir.t) Memo.t

(** Modules attached to a library, executable, or melange.emit stanza.*)
val modules : t -> libs:Lib.DB.t -> for_:for_ -> Modules.t Memo.t

(** Find out the origin of the stanza for a given module *)
val find_origin : t -> libs:Lib.DB.t -> Module_name.Path.t -> Origin.t option Memo.t

val empty : t

(** This [lookup_vlib] argument is required for constructing the collection of
    modules for an implementation of a virtual library.

    We need to know the contents of the virtual library to: - verify conditions
    all virtual modules are implemented - make sure that we construct [Module.t]
    with the correct [kind] *)

val include_subdirs : t -> Include_subdirs.t

val make
  :  Stanza.t list
  -> expander:Expander.t
  -> dir:Path.Build.t
  -> libs:Lib.DB.t Memo.t
  -> project:Dune_project.t
  -> lib_config:Lib_config.t Memo.t
  -> loc:Loc.t
  -> lookup_vlib:(loc:Loc.t -> dir:Path.Build.t -> t Memo.t)
  -> include_subdirs:Loc.t * Include_subdirs.t
  -> dirs:Source_file_dir.t list
  -> t Memo.t
