open Import

val setup_rules
  :  components:string list
  -> dir:Path.Build.t
  -> projects:Dune_project.t list Memo.t
  -> Context_name.t
  -> Build_config.Gen_rules.t Memo.t

val setup_tmp_lock_alias : dir:Path.Build.t -> Context_name.t -> Build_config.Gen_rules.t
