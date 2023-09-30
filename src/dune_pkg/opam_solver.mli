open Import

module Summary : sig
  (** Some intermediate state from the solve exposed for logging purposes *)
  type t

  (** A message listing selected packages *)
  val selected_packages_message : t -> lock_dir_path:Path.Source.t -> User_message.t
end

module Solver_result : sig
  type t =
    { summary : Summary.t
    ; lock_dir : Lock_dir.t
    ; files : Lock_dir.Write_disk.Files_entry.t Package_name.Map.Multi.t
    }
end

val solve_lock_dir
  :  Solver_env.t
  -> Version_preference.t
  -> Opam_repo.t list
  -> local_packages:OpamFile.OPAM.t Package_name.Map.t
  -> (Solver_result.t, [ `Diagnostic_message of _ Pp.t ]) result
