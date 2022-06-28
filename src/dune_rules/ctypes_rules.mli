open Import

val gen_rules :
     cctx:Compilation_context.t
  -> buildable:Dune_file.Buildable.t
  -> loc:Loc.t
  -> scope:Scope.t
  -> dir:Path.Build.t
  -> sctx:Super_context.t
  -> unit Memo.t

val ctypes_cclib_flags :
     Super_context.t
  -> expander:Expander.t
  -> buildable:Dune_file.Buildable.t
  -> string list Action_builder.t
