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

let paths { action_deps; rule_deps } =
  Dep.Set.paths (Dep.Set.union action_deps rule_deps)

let union_map xs ~f =
  let sets = List.map xs ~f in
  { rule_deps = Dep.Set.union_map sets ~f:(fun x -> x.rule_deps)
  ; action_deps = Dep.Set.union_map sets ~f:(fun x -> x.action_deps)
  }
