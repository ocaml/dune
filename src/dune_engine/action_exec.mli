open Import

module Exec_result : sig
  (* Exceptions that can be raised by action execution. We catch those and
     use a variant type so we can marshal them across processes. We lose backtraces,
     but we don't print them for most exceptions. *)
  module Error : sig
    type t =
      | User of User_message.t
      | Code of Code_error.t
      | Sys of string
      | Unix of Unix.error * string * string
      | Nonreproducible_build_cancelled
  end

  type ok =
    { dynamic_deps_stages :
        (* The set can be derived from the facts by getting the keys of the
           facts map. We don't do it because conversion isn't free *)
        (Dep.Set.t * Dep.Facts.t) list
    }

  type t = (ok, Error.t list) Result.t

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
        runs with the total number of job slots in [DUNE_JOBS]. The result is
        limited to the range from 1 to the total number of job slots. [action]
        sees the result in [DUNE_JOB_SLOTS]. Without it, [action] takes one
        job slot. *)
  }

val exec
  :  input
  -> build_deps:(Dep.Set.t -> Dep.Fact.t Dep.Map.t Fiber.t)
  -> Exec_result.t Fiber.t
