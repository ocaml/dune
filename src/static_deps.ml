open! Import

type t =
  { rule_deps   : Dep.Set.t
  ; action_deps : Dep.Set.t
  }

let pp fmt { rule_deps ; action_deps } =
  Fmt.record fmt
    [ "rule_deps", Fmt.const Dep.Set.pp rule_deps
    ; "action_deps", Fmt.const Dep.Set.pp action_deps
    ]

let action_deps t = t.action_deps

let rule_deps t = t.rule_deps

let empty =
  { rule_deps = Dep.Set.empty
  ; action_deps = Dep.Set.empty
  }

let add_rule_paths t fns =
  { t with
    rule_deps = Dep.Set.add_paths t.rule_deps fns
  }

let add_rule_path t fn =
  { t with
    rule_deps = Dep.Set.add t.rule_deps (Dep.file fn)
  }

let add_action_dep t dep =
  { t with
    action_deps = Dep.Set.add t.action_deps dep
  }

let add_action_paths t fns =
  { t with
    action_deps = Dep.Set.add_paths t.action_deps fns
  }

let add_action_env_var t var =
  { t with
    action_deps = Dep.Set.add t.action_deps (Dep.env var)
  }

let paths {action_deps; rule_deps} =
  Dep.Set.paths (Dep.Set.union action_deps rule_deps)
