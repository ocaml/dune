open Import

module Solver_result : sig
  type t =
    { lock_dir : Lock_dir.t
    ; files : File_entry.t Package_version.Map.Multi.t Package_name.Map.t
    ; pinned_packages : Package_name.Set.t
    ; num_expanded_packages : int
    }
end

(** Split [solver_env] into a portable base env (with platform-specific
    variables unset) and the distinct platform overlays to solve for. *)
val base_solver_env_and_platforms
  :  Solver_env.t
  -> solve_for_platforms:Solver_env.t list
  -> Solver_env.t * Solver_env.t list

val solve_lock_dir
  :  Solver_env.t
  -> platform_overlays:Solver_env.t list
  -> Version_preference.t
  -> Opam_repo.t list
  -> local_packages:Local_package.For_solver.t Package_name.Map.t
  -> pins:Resolved_package.t Package_name.Map.t
  -> constraints:Dune_lang.Package_dependency.t list
  -> selected_depopts:Package_name.t list
  -> ( Solver_result.t
       , [ `Solve_error of User_message.Style.t Pp.t | `Manifest_error of User_message.t ]
       )
       result
       Fiber.t
