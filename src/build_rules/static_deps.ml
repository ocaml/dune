open! Import

type t =
  { rule_deps : Dep.Set.t
  ; action_deps : Dep.Set.t
  }

let to_dyn { rule_deps; action_deps } =
  let open Dyn.Encoder in
  record
    [ ("rule_deps", Dep.Set.to_dyn rule_deps)
    ; ("action_deps", Dep.Set.to_dyn action_deps)
    ]

let empty = { rule_deps = Dep.Set.empty; action_deps = Dep.Set.empty }

let union x y =
  { rule_deps = Dep.Set.union x.rule_deps y.rule_deps
  ; action_deps = Dep.Set.union x.action_deps y.action_deps
  }

let paths { action_deps; rule_deps } ~eval_pred =
  Dep.Set.paths (Dep.Set.union action_deps rule_deps) ~eval_pred
