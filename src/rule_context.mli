open Stdune

type t

val context : t -> Context.t

val make
  :  build_system:Build_system.t
  -> env:Env.t
  -> chdir:(Action.t, Action.t) Build.t
  -> context:Context.t
  -> file_tree:File_tree.t
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

val prefix_rules
  :  t
  -> (unit, unit) Build.t
  -> f:(unit -> 'a)
  -> 'a

(** Interpret dependencies written in jbuild files *)
module Deps : sig
  (** Evaluates to the actual list of dependencies, ignoring aliases *)
  val interpret
    :  t
    -> expander:Expander.t
    -> Dune_file.Dep_conf.t list
    -> (unit, Path.t list) Build.t

  val interpret_named
    :  t
    -> expander:Expander.t
    -> Dune_file.Dep_conf.t Bindings.t
    -> (unit, Path.t Bindings.t) Build.t
end

