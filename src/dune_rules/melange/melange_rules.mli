open Import

val emit_target_dir : Melange_stanzas.Emit.t -> dir:Path.Build.t -> Path.Build.t

val setup_emit_cmj_rules :
     sctx:Super_context.t
  -> dir:Path.Build.t
  -> scope:Scope.t
  -> expander:Expander.t
  -> dir_contents:Dir_contents.t
  -> Melange_stanzas.Emit.t
  -> (Compilation_context.t * Merlin.t) Memo.t

val setup_emit_js_rules :
     dir_contents:Dir_contents.t
  -> dir:Path.Build.t
  -> scope:Scope.t
  -> sctx:Super_context.t
  -> Melange_stanzas.Emit.t
  -> unit Memo.t

module Runtime_deps : sig
  type path_specification =
    | Allow_all
    | Disallow_external of Lib_name.t

  val eval :
       loc:Loc.t
    -> expander:Expander.t
    -> paths:path_specification
    -> Dep_conf.t list
    -> Path.Set.t Memo.t
end
