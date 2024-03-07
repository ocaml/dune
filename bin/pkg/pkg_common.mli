open Import

(** Create a [Dune_pkg.Solver_env.t] by combining variables taken from the
    current system and variables taken from the current context, with priority
    being given to the latter. Some variables are initialized to default values
    (which can be overridden by the arguments to this function):
    - "with-doc" is set to "false"
    - "opam-version" is set to the version of opam vendored in dune *)
val solver_env
  :  solver_env_from_current_system:Dune_pkg.Solver_env.t option
  -> solver_env_from_context:Dune_pkg.Solver_env.t option
  -> unset_solver_vars_from_context:Dune_lang.Package_variable_name.Set.t option
  -> Dune_pkg.Solver_env.t

module Version_preference : sig
  type t := Dune_pkg.Version_preference.t

  val term : Dune_pkg.Version_preference.t option Term.t
  val choose : from_arg:t option -> from_context:t option -> t
end

val unset_solver_vars_of_workspace
  :  Workspace.t
  -> lock_dir_path:Path.Source.t
  -> Dune_lang.Package_variable_name.Set.t option

val repositories_of_workspace
  :  Workspace.t
  -> Dune_pkg.Pkg_workspace.Repository.t Dune_pkg.Pkg_workspace.Repository.Name.Map.t

val repositories_of_lock_dir
  :  Workspace.t
  -> lock_dir_path:Path.Source.t
  -> (Loc.t * Dune_pkg.Pkg_workspace.Repository.Name.t) list

val constraints_of_workspace
  :  Workspace.t
  -> lock_dir_path:Path.Source.t
  -> Dune_lang.Package_dependency.t list

val get_repos
  :  Dune_pkg.Pkg_workspace.Repository.t Dune_pkg.Pkg_workspace.Repository.Name.Map.t
  -> repositories:(Loc.t * Dune_pkg.Pkg_workspace.Repository.Name.t) list
  -> Dune_pkg.Opam_repo.t list Fiber.t

val find_local_packages : Dune_pkg.Local_package.t Package_name.Map.t Memo.t

module Lock_dirs_arg : sig
  (** [Lock_dirs_arg.t] is the type of lock directory arguments. This can be
      created with [Lock_dirs_arg.term] and used with
      [Lock_dirs_arg.lock_dirs_of_workspace]. *)
  type t

  (** [Lock_dirs_arg.term] is a command-line argument that can be used to
      specify the lock directories to consider. This can then be passed to
      [Lock_dirs_arg.lock_dirs_of_workspace].

      There are two mutually exclusive cases:
      - The user passed a list of lick directories as positional
        arguments.contents
      - The user passed the ["--all"] flag, in which case all lock directories
        of the workspace are considered. *)
  val term : t Term.t

  (** [Lock_dirs_arg.lock_dirs_of_workspace t workspace] returns the list of
      lock directories that should be considered for various operations.

      The [workspace] argument is used to determine the list of all lock lock
      directories.

      A user error is raised if the list of positional arguments used when
      creating [t] is not a subset of the lock directories of the workspace. *)
  val lock_dirs_of_workspace : t -> Workspace.t -> Path.Source.t list
end

(** [pp_packages lock_dir] returns a list of pretty-printed packages occurring in
    [lock_dir]. *)
val pp_packages : Dune_pkg.Lock_dir.Pkg.t list -> 'a Pp.t
