open Import

module Artifacts : sig
  type t

  val lookup_module :
    t -> Module_name.t -> (Path.Build.t Obj_dir.t * Module.t) option

  val lookup_library : t -> Lib_name.t -> Dune_file.Library.t option
end

type t

val artifacts : t -> Artifacts.t

(** Modules attached to a library. [name] is the library best name. *)
val modules_of_library : t -> name:Lib_name.t -> Modules.t

(** Modules attached to a set of executables. *)
val modules_of_executables :
  t -> obj_dir:Path.Build.t Obj_dir.t -> first_exe:string -> Modules.t

(** Find out what buildable a module is part of *)
val lookup_module : t -> Module_name.t -> Dune_file.Buildable.t option

val standalone :
     Stanza.t list Dir_with_dune.t
  -> files:String.Set.t
  -> parent:(dir:Path.Build.t -> t)
  -> t

val empty : t

val group :
     Stanza.t list Dir_with_dune.t
  -> loc:Loc.t
  -> parent:(dir:Path.Build.t -> t)
  -> include_subdirs:Dune_file.Include_subdirs.t
  -> dir:Path.Build.t
  -> files:String.Set.t
  -> subdirs:(Path.Build.t * 'a list * String.Set.t) list
  -> t
