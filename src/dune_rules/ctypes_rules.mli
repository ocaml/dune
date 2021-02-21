open! Stdune

val gen_rules :
     scope:Scope.t
  -> expander:Expander.t
  -> dir:Path.Build.t
  -> sctx:Super_context.t
  -> ctypes_library:Dune_file.Ctypes_library.t
  -> unit
