open Stdune

module Alias = Build_system.Alias

type t =
  { build_system : Build_system.t
  ; env : Env.t
  ; chdir : (Action.t, Action.t) Build.t
  ; context : Context.t
  }

let make ~build_system ~env ~chdir ~context =
  { build_system
  ; env
  ; chdir
  ; context
  }

let add_rule t ?sandbox ?mode ?locks ?loc build =
  let build = Build.O.(>>>) build t.chdir in
  Build_system.add_rule t.build_system
    (Build_interpret.Rule.make ?sandbox ?mode ?locks ?loc
       ~context:(Some t.context) ~env:(Some t.env) build)

let add_rule_get_targets t ?sandbox ?mode ?locks ?loc build =
  let build = Build.O.(>>>) build t.chdir in
  let rule =
    Build_interpret.Rule.make ?sandbox ?mode ?locks ?loc
      ~context:(Some t.context) ~env:(Some t.env) build
  in
  Build_system.add_rule t.build_system rule;
  List.map rule.targets ~f:Build_interpret.Target.path

let add_rules t ?sandbox builds =
  List.iter builds ~f:(add_rule t ?sandbox)

let add_alias_deps t alias ?dyn_deps deps =
  Alias.add_deps t.build_system alias ?dyn_deps deps

let add_alias_action t alias ~loc ?locks ~stamp action =
  Alias.add_action t.build_system ~context:t.context ~env:(Some t.env)
    alias ~loc ?locks ~stamp action
