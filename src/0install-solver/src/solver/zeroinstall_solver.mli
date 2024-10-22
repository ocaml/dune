(* Copyright (C) 2020, Thomas Leonard
   See the README file for details, or visit http://0install.net. *)

module S = S

(** Select a compatible set of components to run a program.
    See [Zeroinstall.Solver] for the instantiation of this functor on the
    actual 0install types. *)
module Make(Monad : S.Monad)(Input : S.SOLVER_INPUT with type 'a monad = 'a Monad.t) : sig
  module Output : S.SOLVER_RESULT with module Input = Input

  (** [do_solve model req] finds an implementation matching the given requirements, plus any other implementations needed
      to satisfy its dependencies.
      @param closest_match adds a lowest-ranked (but valid) implementation ([Input.dummy_impl]) to
        every interface, so we can always select something. Useful for diagnostics.
        You should ensure that [Input.get_command] always returns a dummy command for dummy_impl too.
        Note: always try without [closest_match] first, or it may miss a valid solution.
      @return None if the solve fails (only happens if [closest_match] is false). *)
  val do_solve : closest_match:bool -> Input.requirements -> Output.t option Monad.t
end

(** Explaining why a solve failed or gave an unexpected answer. *)
module Diagnostics(Monad : S.Monad)(Result : S.SOLVER_RESULT with type 'a Input.monad := 'a Monad.t) : sig

  (** An item of information to display for a component. *)
  module Note : sig
    type t =
      | UserRequested of Result.Input.restriction
      | ReplacesConflict of Result.Role.t
      | ReplacedByConflict of Result.Role.t
      | Restricts of Result.Role.t * Result.Input.impl * Result.Input.restriction list
      | RequiresCommand of Result.Role.t * Result.Input.impl * Result.Input.command_name
      | Feed_problem of string

    val pp : Format.formatter -> t -> unit
  end

  (** Information about a single role in the example (failed) selections produced by the solver. *)
  module Component : sig
    type t

    type rejection_reason = [
      | `Model_rejection of Result.Input.rejection      (** Rejected before getting to solver. *)
      | `FailsRestriction of Result.Input.restriction   (** e.g. version too old for another component. *)
      | `DepFailsRestriction of Result.Input.dependency * Result.Input.restriction (** Couldn't satisfy its dependencies. *)
      | `MachineGroupConflict of Result.Role.t * Result.Input.impl (** A selected impl has a different machine type. *)
      | `ClassConflict of Result.Role.t * Result.Input.conflict_class (** A selected impl has the same conflict class. *)
      | `ConflictsRole of Result.Role.t                 (** A selected role conflicts with this (e.g. replaced-by). *)
      | `MissingCommand of Result.Input.command_name    (** Doesn't have a command we need. *)
      | `DiagnosticsFailure of string                   (** Unknown failure reason (gives raw error from SAT solver). *)
    ]
    (** Why a particular implementation was rejected. This could be because the
        input rejected it before it got to the solver, or because it conflicts
        with something else in the example (partial) solution. *)

    type reject = Result.Input.impl * rejection_reason

    val pp_reject : Format.formatter -> reject -> unit

    val selected_impl : t -> Result.Input.impl option
    (** [selected_impl t] is the implementation selected to fill [t]'s role, or
        [None] if no implementation was suitable. *)

    val notes : t -> Note.t list
    (** Information discovered about this component. *)

    val rejects : t -> reject list * [ `All_unusable | `No_candidates | `Conflicts ]
    (** [rejects t] returns the rejected candidates (call this if [selected_impl t = None]).
        [`No_candidates] means that there were no implementations given at all (e.g. bad role name).
        [`All_unusable] means every candidate was rejected before reaching the solver (e.g. they were
        all binaries for some other platform).
        [`Conflicts] is the normal case, where some made it to the solver, but were rejected because
        they conflicted with other selections. *)

    val pp : verbose:bool -> Format.formatter -> t -> unit
    (** [pp ~verbose] formats a message showing the status of this component,
        including all of its notes and, if there was no selected impl, the rejects.
        @param verbose If [false], limit the list of rejected candidates (if any) to five entries. *)
  end

  type t = Component.t Result.RoleMap.t
  (** An analysis of why the solve failed. *)

  val of_result : Result.t -> t Monad.t
  (** [of_result r] is an analysis of failed solver result [r].
      We take the partial solution from the solver and discover, for each
      component we couldn't select, which constraints caused the candidates to
      be rejected. *)

  val get_failure_reason : ?verbose:bool -> Result.t -> string Monad.t
  (** [get_failure_reason r] analyses [r] with [of_result] and formats the
      analysis as a string. *)
end

(** The low-level SAT solver. *)
module Sat = Sat
