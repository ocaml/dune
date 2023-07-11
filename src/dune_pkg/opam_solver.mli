open! Stdune

module Summary : sig
  (** Some intermediate state from the solve exposed for logging purposes *)
  type t

  (** A message listing selected packages *)
  val selected_packages_message :
    t -> lock_dir_path:Path.Source.t -> User_message.t
end

val solve_lock_dir :
     solver_env:Solver_env.t
  -> version_preference:Version_preference.t
  -> repo:Opam_repo.t
  -> OpamFile.OPAM.t OpamTypes.name_map
  -> Summary.t * Lock_dir.t
