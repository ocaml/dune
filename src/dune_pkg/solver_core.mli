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
