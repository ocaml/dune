open Import

val action : Path.t -> Path.Build.t -> Action.t

val builder :
  src:Path.t -> dst:Path.Build.t -> Action.Full.t Action_builder.With_targets.t
