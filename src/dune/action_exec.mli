open! Stdune

type done_or_more_deps =
  | Done
  (* TODO jstaron: There is a problem with having a Set/Map of
    [Protocol.Dependency.t]. Assume action have multiple 'dynamic-run' commands
     that are executed in different directories (by chdir). Assume also, that
     they use dependencies with the same names (but different directories).
     Then we can not distinguish these two. *)
  | Need_more_deps of Dep.t Dune_action.Protocol.Dependency.Map.t

val exec :
     targets:Path.Build.Set.t
  -> context:Context.t option
  -> env:Env.t
  -> rule_loc:Loc.t
  -> prepared_dependencies:Dune_action.Protocol.Dependency.Set.t
  -> Action.t
  -> done_or_more_deps Fiber.t
