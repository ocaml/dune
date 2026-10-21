open Import

module Exec_result : sig
  type ok =
    { dynamic_deps_stages :
        (* The set can be derived from the facts by getting the keys of the
           facts map. We don't do it because conversion isn't free *)
        (Dep.Set.t * Dep.Facts.t) list
    }

  type t = (ok, Exn_with_backtrace.t list) Result.t

  val ok_exn : t -> ok Fiber.t
end

type input =
  { targets : Targets.Validated.t option (* Some Jane Street actions use [None] *)
  ; root : Path.t
    (** [root] should be the root of the current build context, or the root
      of the sandbox if the action is sandboxed. *)
  ; context : Build_context.t option
  ; env : Env.t
  ; rule_loc : Loc.t
  ; execution_parameters : Execution_parameters.t
  ; sandbox : Process.Sandbox.t option
  ; action : Action.t
  ; job_slots : Action.t option
    (** An action that prints the number of job slots that [action] takes. It
        runs with the total number of job slots in [DUNE_JOBS]. The result must
        be a positive integer and is capped at the total number of job slots.
        [action] sees the result in [DUNE_JOB_SLOTS]. Without it, [action] takes
        one job slot. *)
  }

val exec
  :  input
  -> build_deps:(Dep.Set.t -> Dep.Fact.t Dep.Map.t Fiber.t)
  -> Exec_result.t Fiber.t
