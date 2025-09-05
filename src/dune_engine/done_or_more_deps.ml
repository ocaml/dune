module Dependency = Dune_action_plugin.Private.Protocol.Dependency

type t =
  | Done
  | Need_more_deps of (Dependency.Set.t * Dep.Set.t)

let union (x : t) (y : t) =
  match x, y with
  | Done, Done -> Done
  | Done, Need_more_deps x | Need_more_deps x, Done -> Need_more_deps x
  | Need_more_deps (deps1, dyn_deps1), Need_more_deps (deps2, dyn_deps2) ->
    Need_more_deps (Dependency.Set.union deps1 deps2, Dep.Set.union dyn_deps1 dyn_deps2)
;;
