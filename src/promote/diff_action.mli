open Import

val diff
  :  ?optional:bool
  -> ?mode:Diff.Mode.t
  -> ?force_source:bool
  -> Path.t
  -> Path.Build.t
  -> Dune_engine.Action.t
