open Import

type t

val empty : t

val make
  :  dir:Path.Build.t
  -> expander:Expander0.t
  -> lib_config:Lib_config.t Memo.t
  -> libs:(Library.t * Modules.t * Path.Build.t Obj_dir.t) list
  -> exes:(Modules.t * Path.Build.t Obj_dir.t) list
  -> t Memo.t

val lookup_module : t -> Module_name.t -> (Path.Build.t Obj_dir.t * Module.t) option
val lookup_library : t -> Lib_name.t -> Lib_info.local option
