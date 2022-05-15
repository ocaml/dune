open Import

val generated_ml_and_c_files : Ctypes_stanza.t -> string list

val non_installable_modules : Ctypes_stanza.t -> Module_name.t list

val gen_rules :
     cctx:Compilation_context.t
  -> buildable:Dune_file.Buildable.t
  -> loc:Loc.t
  -> scope:Scope.t
  -> dir:Path.Build.t
  -> sctx:Super_context.t
  -> unit Memo.t

val ctypes_cclib_flags :
     standard:string list Action_builder.t
  -> scope:Scope.t
  -> expander:Expander.t
  -> buildable:Dune_file.Buildable.t
  -> string list Action_builder.t
