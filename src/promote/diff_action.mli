open Import

val diff
  :  ?optional:bool
  -> ?mode:Diff.Mode.t
  -> Path.t
  -> Path.Build.t
  -> Dune_engine.Action.t
