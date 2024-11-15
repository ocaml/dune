(* Copyright (C) 2020, Thomas Leonard
   See the README file for details, or visit http://0install.net. *)

module S = S

(** Select a compatible set of components to run a program.
    See [Zeroinstall.Solver] for the instantiation of this functor on the
    actual 0install types. *)
module Make (Input : S.SOLVER_INPUT) : sig
  module Output : S.SOLVER_RESULT with module Input = Input

  (** [do_solve model req] finds an implementation matching the given requirements, plus any other implementations needed
      to satisfy its dependencies.
      @param closest_match
        adds a lowest-ranked (but valid) implementation ([Input.dummy_impl]) to
        every interface, so we can always select something. Useful for diagnostics.
        Note: always try without [closest_match] first, or it may miss a valid solution.
      @return None if the solve fails (only happens if [closest_match] is false). *)
  val do_solve : closest_match:bool -> Input.Role.t -> Output.t option Fiber.t
end

(** Explaining why a solve failed or gave an unexpected answer. *)
module Diagnostics (Result : S.SOLVER_RESULT) : sig
  (** An item of information to display for a component. *)
  module Note : sig
    type t =
      | UserRequested of Result.Input.restriction
      | ReplacesConflict of Result.Role.t
      | ReplacedByConflict of Result.Role.t
      | Restricts of Result.Role.t * Result.Input.impl * Result.Input.restriction list
      | Feed_problem of string

    val pp : t -> 'tag Pp.t
  end

  (** Information about a single role in the example (failed) selections produced by the solver. *)
  module Component : sig
    type t

    (** Why a particular implementation was rejected. This could be because the
        input rejected it before it got to the solver, or because it conflicts
        with something else in the example (partial) solution. *)
    type rejection_reason =
      [ `Model_rejection of Result.Input.rejection
        (** Rejected before getting to solver. *)
      | `FailsRestriction of Result.Input.restriction
        (** e.g. version too old for another component. *)
      | `DepFailsRestriction of Result.Input.dependency * Result.Input.restriction
        (** Couldn't satisfy its dependencies. *)
      | `ClassConflict of Result.Role.t * Result.Input.conflict_class
        (** A selected impl has the same conflict class. *)
      | `ConflictsRole of Result.Role.t
        (** A selected role conflicts with this (e.g. replaced-by). *)
      | `DiagnosticsFailure of Stdune.User_message.Style.t Pp.t
        (** Unknown failure reason (gives raw error from SAT solver). *)
      ]

    type reject = Result.Input.impl * rejection_reason

    val pp_reject : reject -> Stdune.User_message.Style.t Pp.t

    (** [selected_impl t] is the implementation selected to fill [t]'s role, or
        [None] if no implementation was suitable. *)
    val selected_impl : t -> Result.Input.impl option

    (** Information discovered about this component. *)
    val notes : t -> Note.t list

    (** [rejects t] returns the rejected candidates (call this if [selected_impl t = None]).
        [`No_candidates] means that there were no implementations given at all (e.g. bad role name).
        [`All_unusable] means every candidate was rejected before reaching the solver (e.g. they were
        all binaries for some other platform).
        [`Conflicts] is the normal case, where some made it to the solver, but were rejected because
        they conflicted with other selections. *)
    val rejects : t -> reject list * [ `All_unusable | `No_candidates | `Conflicts ]

    (** [pp ~verbose] formats a message showing the status of this component,
        including all of its notes and, if there was no selected impl, the rejects.
        @param verbose
          If [false], limit the list of rejected candidates (if any) to five entries. *)
    val pp : verbose:bool -> t -> Stdune.User_message.Style.t Pp.t
  end

  (** An analysis of why the solve failed. *)
  type t = Component.t Result.RoleMap.t

  (** [of_result r] is an analysis of failed solver result [r].
      We take the partial solution from the solver and discover, for each
      component we couldn't select, which constraints caused the candidates to
      be rejected. *)
  val of_result : Result.t -> t Fiber.t

  (** [get_failure_reason r] analyses [r] with [of_result] and formats the
      analysis as a string. *)
  val get_failure_reason
    :  ?verbose:bool
    -> Result.t
    -> Stdune.User_message.Style.t Pp.t Fiber.t
end

(** The low-level SAT solver. *)
module Sat = Sat
