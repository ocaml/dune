open Import

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

type input =
  { targets :
      Targets.Validated.t option (* Some Jane Street actions use [None] *)
  ; root : Path.t
        (** [root] should be the root of the current build context, or the root
            of the sandbox if the action is sandboxed. *)
  ; context : Build_context.t option
  ; env : Env.t
  ; rule_loc : Loc.t
  ; execution_parameters : Execution_parameters.t
  ; action : Action.t
  }

val exec :
     input
  -> build_deps:(Dep.Set.t -> Dep.Fact.t Dep.Map.t Fiber.t)
  -> Exec_result.t Fiber.t
