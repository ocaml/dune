open Stdune

type t

val make
  : build_system:Build_system.t
  -> env:Env.t
  -> chdir:(Action.t, Action.t) Build.t
  -> context:Context.t
  -> t

val add_rule
  :  t
  -> ?sandbox:bool
  -> ?mode:Dune_file.Rule.Mode.t
  -> ?locks:Path.t list
  -> ?loc:Loc.t
  -> (unit, Action.t) Build.t
  -> unit
val add_rule_get_targets
  :  t
  -> ?sandbox:bool
  -> ?mode:Dune_file.Rule.Mode.t
  -> ?locks:Path.t list
  -> ?loc:Loc.t
  -> (unit, Action.t) Build.t
  -> Path.t list
val add_rules
  :  t
  -> ?sandbox:bool
  -> (unit, Action.t) Build.t list
  -> unit
val add_alias_deps
  :  t
  -> Build_system.Alias.t
  -> ?dyn_deps:(unit, Path.Set.t) Build.t
  -> Path.Set.t
  -> unit
val add_alias_action
  :  t
  -> Build_system.Alias.t
  -> loc:Loc.t option
  -> ?locks:Path.t list
  -> stamp:_
  -> (unit, Action.t) Build.t
  -> unit
