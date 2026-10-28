open Import

val gen_rules
  :  cctx:Compilation_context.t
  -> buildable:Buildable.t
  -> loc:Loc.t
  -> dir:Path.Build.t
  -> dir_contents:Dir_contents.t
  -> sctx:Super_context.t
  -> unit Memo.t

val ctypes_cclib_flags
  :  Super_context.t
  -> expander:Expander.t
  -> buildable:Buildable.t
  -> string list Action_builder.t
