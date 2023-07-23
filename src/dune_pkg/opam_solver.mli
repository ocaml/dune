open! Stdune

module Summary : sig
  (** Some intermediate state from the solve exposed for logging purposes *)
  type t

  (** A message listing selected packages *)
  val selected_packages_message :
    t -> lock_dir_path:Path.Source.t -> User_message.t
end

val solve_lock_dir :
     Solver_env.t
  -> Version_preference.t
  -> Opam_repo.t
  -> local_packages:OpamFile.OPAM.t OpamTypes.name_map
  -> (Summary.t * Lock_dir.t, [ `Diagnostic_message of _ Pp.t ]) result
