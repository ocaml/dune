open Import

module Solver_result : sig
  type t =
    { lock_dir : Lock_dir.t
    ; files : File_entry.t Package_version.Map.Multi.t Package_name.Map.t
    ; pinned_packages : Package_name.Set.t
    ; num_expanded_packages : int
    }

  val merge : t -> t -> t
end

val solve_lock_dir
  :  Solver_env.t
  -> Version_preference.t
  -> Opam_repo.t list
  -> local_packages:Local_package.For_solver.t Package_name.Map.t
  -> pins:Resolved_package.t Package_name.Map.t
  -> constraints:Dune_lang.Package_dependency.t list
  -> selected_depopts:Package_name.t list
  -> portable_lock_dir:bool
       (** XXX(steve): Indicates that the solver should generate portable
           lockdirs. We try to avoid having the solver behave differently
           depending on whether we are generating portable lockdirs but
           allowing minor changes in what information is stored in the lockdir
           depending on this argument can greatly simplify the logic for
           handling both portable and non-portable lockdirs with the same code.
           Once portable lockdirs are enabled unconditionally, remove this
           argument. *)
  -> ( Solver_result.t
       , [ `Solve_error of User_message.Style.t Pp.t | `Manifest_error of User_message.t ]
       )
       result
       Fiber.t
