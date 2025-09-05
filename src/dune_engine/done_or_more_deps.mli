module Dependency := Dune_action_plugin.Private.Protocol.Dependency

type t =
  | Done
  (* This code assumes that there can be at most one 'dynamic-run' within single
      action. [DAP.Dependency.t] stores relative paths so name clash would be
      possible if multiple 'dynamic-run' would be executed in different
      subdirectories that contains targets having the same name. *)
  | Need_more_deps of (Dependency.Set.t * Dep.Set.t)

val union : t -> t -> t
