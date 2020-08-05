open Stdune

val add_obj_dir : Super_context.t -> obj_dir:Path.Build.t Obj_dir.t -> unit

val add_files : Super_context.t -> dir:Path.Build.t -> Path.t list -> unit
