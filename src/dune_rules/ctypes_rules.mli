open! Stdune

val gen_rules :
     buildable:Dune_file.Buildable.t
  -> dynlink:bool
  -> loc:Loc.t
  -> obj_dir:Path.Build.t Obj_dir.t
  -> scope:Scope.t
  -> expander:Expander.t
  -> dir:Path.Build.t
  -> sctx:Super_context.t
  -> unit
