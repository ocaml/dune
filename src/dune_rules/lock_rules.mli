open Import

val setup_rules
  :  components:string list
  -> dir:Path.Build.t
  -> Build_config.Gen_rules.t Memo.t

val setup_lock_alias : dir:Path.Build.t -> Build_config.Gen_rules.t

val setup_tmp_ocamlformat_alias
  :  dir:Path.Build.t
  -> Context_name.t
  -> Build_config.Gen_rules.t
