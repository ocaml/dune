open! Stdune

val gen_rules :
     base_lib:Dune_file.Library.t
  -> scope:Scope.t
  -> expander:Expander.t
  -> dir:Path.Build.t
  -> sctx:Super_context.t
  -> unit
