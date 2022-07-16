open Import

val add_obj_dir :
  Super_context.t -> obj_dir:Path.Build.t Obj_dir.t -> unit Memo.t

val add_files :
  Super_context.t -> dir:Path.Build.t -> Path.t list -> unit Memo.t

val add_cycle_check :
     Super_context.t
  -> dir:Path.Build.t
  -> Module.t list Action_builder.t
  -> unit Memo.t
