open Import

val rules : Super_context.t -> dir:Path.Build.t -> unit Memo.t
val setup_tmp_lock_alias : dir:Path.Build.t -> Context_name.t -> Build_config.Gen_rules.t
