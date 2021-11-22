open! Stdune

(* For registering the cram_exec function. *)
val cram_run : (env:Env.t -> script:Path.t -> unit Fiber.t) Fdecl.t

(** Type for dependency requested by the dynamic action.

    Must be different from [Dep.t] type because we require it to be
    marshallable. *)
module Dynamic_dep : sig
  type t =
    | File of Path.t
    | Glob of Path.t * Glob.t

  val to_dep : t -> Dep.t

  val compare : t -> t -> Ordering.t

  module Map : Map.S with type key := t

  module Set : sig
    include Set.S with type elt = t and type 'a map = 'a Map.t

    val to_dep_set : t -> Dep.Set.t
  end
end

module Exec_result : sig
  type t = { dynamic_deps_stages : (Dynamic_dep.Set.t * Dep.Facts.t) List.t }
end

(** [root] should be the root of the current build context, or the root of the
    sandbox if the action is sandboxed. *)
val exec :
     targets:Targets.Validated.t option (* Some Jane Street actions use [None] *)
  -> root:Path.t
  -> context:Build_context.t option
  -> env:Env.t
  -> rule_loc:Loc.t
  -> build_deps:(Dep.Set.t -> Dep.Fact.t Dep.Map.t Fiber.t)
  -> execution_parameters:Execution_parameters.t
  -> Action.t
  -> Exec_result.t Fiber.t

(** The [BUILD_PATH_PREFIX_MAP] variable *)
val _BUILD_PATH_PREFIX_MAP : string

(** [extend_build_path_prefix_map env how map] extends the path rewriting rules
    encoded in the [BUILD_PATH_PREFIX_MAP] variable.

    Note that the rewriting rules are applied from right to left, so the last
    rule of [map] will be tried first.

    If the environment variable is already defined in [env], [how] explains
    whether the rules in [map] should be tried before or after the existing
    ones. *)
val extend_build_path_prefix_map :
     Env.t
  -> [ `Existing_rules_have_precedence | `New_rules_have_precedence ]
  -> Build_path_prefix_map.map
  -> Env.t
