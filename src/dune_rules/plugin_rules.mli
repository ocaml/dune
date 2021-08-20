open! Stdune

val setup_rules :
     sctx:Super_context.t
  -> dir:Path.Build.t
  -> Dune_file.Plugin.t
  -> unit Memo.Build.t

val install_rules :
     sctx:Super_context.t
  -> dir:Path.Build.t
  -> Dune_file.Plugin.t
  -> Dune_engine.Install.Entry.Sourced.t list Memo.Build.t
