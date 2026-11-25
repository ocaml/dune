open Import

module Exec_result : sig
  type t = (unit, Exn_with_backtrace.t list) Result.t

  val ok_exn : t -> unit Fiber.t
end

type input =
  { targets : Targets.Validated.t option (* Some Jane Street actions use [None] *)
  ; root : Path.t
    (** [root] should be the root of the current build context, or the root
      of the sandbox if the action is sandboxed. *)
  ; env : Env.t
  ; rule_loc : Loc.t
  ; execution_parameters : Execution_parameters.t
  ; sandbox : Process.Sandbox.t option
  ; action : Action.t
  ; job_slots : int
  }

val exec : input -> build_deps:(Dep.Set.t -> unit Fiber.t) -> Exec_result.t Fiber.t
