open! Stdune

type done_or_more_deps =
  | Done
  | Need_more_deps of Dep.t Dune_action.Protocol.Dependency.Map.t

val exec :
     targets:Path.Build.Set.t
  -> context:Context.t option
  -> env:Env.t
  -> rule_loc:Loc.t
  -> provided_dependencies:Dune_action.Protocol.Dependency.Set.t
  -> Action.t
  -> done_or_more_deps Fiber.t
