open Import

val gen_rules
  :  sctx:Super_context.t
  -> dir:Path.Build.t
  -> scope:Scope.t
  -> string list
  -> Build_config.Gen_rules.result Memo.t
