open Import

module Solver_result : sig
  type t =
    { lock_dir : Lock_dir.t
    ; files : File_entry.t Package_name.Map.Multi.t
    ; pinned_packages : Package_name.Set.t
    }
end

val solve_lock_dir
  :  Solver_env.t
  -> Version_preference.t
  -> Opam_repo.t list
  -> local_packages:Local_package.For_solver.t Package_name.Map.t
  -> pins:Resolved_package.t Package_name.Map.t
  -> constraints:Dune_lang.Package_dependency.t list
  -> (Solver_result.t, [ `Diagnostic_message of _ Pp.t ]) result Fiber.t
