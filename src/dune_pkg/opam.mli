open Stdune

module Repo : sig
  (** An opam repository *)
  type t

  (** [of_opam_repo_dir_path opam_repo_dir] creates a repo representedy by a
      local directory in the path given by [opam_repo_dir]. *)
  val of_opam_repo_dir_path : Path.t -> t
end

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
  -> repo:Repo.t
  -> OpamFile.OPAM.t OpamTypes.name_map
  -> Summary.t * Lock_dir.t
