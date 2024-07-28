open Import
module DAP := Dune_action_plugin.Private.Protocol

type done_or_more_deps =
  | Done
  (* This code assumes that there can be at most one 'dynamic-run' within single
     action. [DAP.Dependency.t] stores relative paths so name clash would be
     possible if multiple 'dynamic-run' would be executed in different
     subdirectories that contains targets having the same name. *)
  | Need_more_deps of (DAP.Dependency.Set.t * Dep.Set.t)

val to_dune_dep_set : DAP.Dependency.Set.t -> loc:Loc.t -> working_dir:Path.t -> Dep.Set.t
val done_or_more_deps_union : done_or_more_deps -> done_or_more_deps -> done_or_more_deps
