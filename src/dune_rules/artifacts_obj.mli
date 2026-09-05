open Import

type t

val empty : t

val make
  :  dir:Path.Build.t
  -> expander:Expander0.t
  -> lib_config:Lib_config.t Memo.t
  -> libs:(Library.t * Modules.t * Path.Build.t Obj_dir.t) list
  -> exes:(Modules.t * Path.Build.t Obj_dir.t) list
  -> include_subdirs:Include_subdirs.t
  -> melange_emits:(Path.Build.t * (Melange.Emit.t * Loc.t)) list
  -> t Memo.t

val lookup_module_by_source_path
  :  t
  -> Path.Build.t
  -> (Path.Build.t Obj_dir.t * Module.t) option

val lookup_modules_by_logical_path
  :  t
  -> Module_name.Path.t
  -> (Path.Build.t Obj_dir.t * Module.t) list

val include_subdirs : t -> Include_subdirs.t
val lookup_library : t -> Lib_name.t -> Lib_info.local option
val lookup_melange_emit : t -> Path.Build.t -> Melange.Emit.t option
