module Make (C : S.CONTEXT) : sig
  module Input : Zeroinstall_solver.S.SOLVER_INPUT with type rejection = C.rejection

  module Solver : sig
    module Output : Zeroinstall_solver.S.SOLVER_RESULT with module Input = Input
  end

  include S.SOLVER with type t = C.t and type selections = Solver.Output.t

  module Diagnostics : sig
    include module type of Zeroinstall_solver.Diagnostics (Solver.Output)
  end

  val diagnostics_rolemap : diagnostics -> Diagnostics.t Fiber.t
end
