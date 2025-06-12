open Import

(* val lock : target:Path.Build.t -> lock_dir:string -> Action.Full.t With_targets.t *)

val setup_rules
  :  components:string list
  -> dir:Path.Build.t (* -> Super_context.t Memo.t *)
  -> Context_name.t
  -> Build_config.Gen_rules.t Memo.t

val setup_tmp_lock_alias : dir:Path.Build.t -> Context_name.t -> Build_config.Gen_rules.t
