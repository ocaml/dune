open Stdune

module Env : sig
  (** An opam environment consisting of assignments to variables (e.g. "arch"
      and "os") *)
  type t

  (** An environment containing no variables *)
  val empty : t

  (** Create an environment matching that of the global opam installation. *)
  val global : unit -> t
end

module Repo_selection : sig
  (** An opam repository *)
  type t

  val switch_with_name : string -> t

  val local_repo_with_env : opam_repo_dir_path:Filename.t -> env:Env.t -> t
end

module Summary : sig
  (** Some intermediate state from the solve exposed for logging purposes *)
  type t

  (** A message listing selected packages *)
  val selected_packages_message : t -> User_message.t
end

val solve_lock_dir :
     repo_selection:Repo_selection.t
  -> lock_dir_path:Path.Source.t
  -> OpamFile.OPAM.t OpamTypes.name_map
  -> Summary.t * Lock_dir.t
