open Stdune

module Repo : sig
  (** An opam repository *)
  type t

  (** Create a [t] from a path to a local directory containing a opam
      repository. Raises an exception if the directory is not a valid opam
      repository. *)
  val of_opam_repo_dir_path : Filename.t -> t
end

module Env : sig
  (** An opam environment consisting of assignments to variables (e.g. "arch"
      and "os") *)
  type t

  (** An environment containing no variables *)
  val empty : t

  (** Create an environment matching that of the global opam installation. *)
  val global : unit -> t
end

module Summary : sig
  (** Some intermediate state from the solve exposed for logging purposes *)
  type t

  (** A message listing selected packages *)
  val selected_packages_message : t -> User_message.t
end

val solve_lock_dir :
     env:Env.t
  -> repo:Repo.t
  -> lock_dir_path:Path.Source.t
  -> OpamFile.OPAM.t OpamTypes.name_map
  -> Summary.t * Lock_dir.t
