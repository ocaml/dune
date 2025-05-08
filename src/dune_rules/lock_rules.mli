open Import

val lock : target:Path.Build.t -> lock_dir:string -> Action.Full.t With_targets.t
