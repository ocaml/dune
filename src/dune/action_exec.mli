open! Stdune

type done_or_more_deps =
  | Done
  (* This code assumes that there can be at most one 'dynamic-run' within
    single action. [Dune_action.Protocol.Dependency.t] stores relative paths so
     name clash would possible if multiple 'dynamic-run' would be executed in
     different subdirectories that contains targets having the same name. *)
  | Need_more_deps of Dep.t Dune_action.Protocol.Dependency.Map.t

val exec :
     targets:Path.Build.Set.t
  -> context:Context.t option
  -> env:Env.t
  -> rule_loc:Loc.t
  -> prepared_dependencies:Dune_action.Protocol.Dependency.Set.t
  -> Action.t
  -> done_or_more_deps Fiber.t
