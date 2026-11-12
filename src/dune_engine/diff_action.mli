open Import

val exec
  :  promotion:Execution_parameters.Promotion.t
  -> sandbox:Process.Sandbox.t option
  -> Loc.t
  -> patch_back:Path.t option
  -> (Path.t, Path.Build.t) Stdune.Action_types.Diff.t
  -> unit Fiber.t
