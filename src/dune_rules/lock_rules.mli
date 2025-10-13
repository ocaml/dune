open Import

val setup_rules
  :  components:string list
  -> dir:Path.Build.t
  -> Build_config.Gen_rules.t Memo.t

val setup_lock_alias : dir:Path.Build.t -> Build_config.Gen_rules.t
