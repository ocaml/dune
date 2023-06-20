open Import

module Exec_result : sig
  type t =
    { dynamic_deps_stages :
        (* The set can be derived from the facts by getting the keys of the
           facts map. We don't do it because conversion isn't free *)
        (Dep.Set.t * Dep.Facts.t) list
    }
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
