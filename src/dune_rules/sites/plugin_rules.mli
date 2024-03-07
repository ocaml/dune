open Import

val setup_rules : sctx:Super_context.t -> dir:Path.Build.t -> Plugin.t -> unit Memo.t

val install_rules
  :  sctx:Super_context.t
  -> package_db:Package_db.t
  -> dir:Path.Build.t
  -> Plugin.t
  -> Install.Entry.Sourced.t list Memo.t
