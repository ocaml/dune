open Import

val exec
  :  Loc.t
  -> patch_back:Path.t option
  -> (Path.t, Path.Build.t) Dune_util.Action.Diff.t
  -> unit Fiber.t
