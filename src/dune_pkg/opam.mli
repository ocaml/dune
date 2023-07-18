open Stdune

module Env : sig
  (** An opam environment consisting of assignments to variables (e.g. "arch"
      and "os") *)
  type t

  (** An environment containing no variables *)
  val empty : t

  (** Create an environment matching that of the global opam installation. *)
  val global : unit -> t

  (** Adds a value to the environment *)
  val add : var:Env.Var.t -> value:string -> t -> t

  (** [union l r] merges two environments together. On key clashes prefers the
      value from [r]. *)
  val union : t -> t -> t
end

module Repo_selection : sig
  (** An opam repository *)
  type t

  (** An opam repo in a local directory given by [opam_repo_dir]. *)
  val local_repo_with_env : opam_repo_dir:Path.t -> env:Env.t -> t
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
  -> repo_selection:Repo_selection.t
  -> OpamFile.OPAM.t OpamTypes.name_map
  -> Summary.t * Lock_dir.t
