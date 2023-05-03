open Import

val compile_info :
  scope:Scope.t -> Dune_file.Executables.t -> Lib.Compile.t Memo.t

val rules :
     ?lib_to_entry_modules_map:(Lib.t * Module.t list) list Resolve.Memo.t
  -> ?lib_top_module_map:
       (Module_name.t * Module.t list) list list Resolve.Memo.t
  -> sctx:Super_context.t
  -> dir:Path.Build.t
  -> dir_contents:Dir_contents.t
  -> scope:Scope.t
  -> expander:Expander.t
  -> Dune_file.Executables.t
  -> (Compilation_context.t * Merlin.t) Memo.t
