open Import

val context_term : doc:string -> Context_name.t option Term.t

(** Create a [Dune_pkg.Solver_env.t] by combining variables taken from the
    current system and variables taken from the current context, with priority
    being given to the latter. Some variables are initialized to default values
    (which can be overridden by the arguments to this function):
    - "with-doc" is set to "false"
    - "opam-version" is set to the version of opam vendored in dune *)
val solver_env
  :  solver_env_from_current_system:Dune_pkg.Solver_env.t option
  -> solver_env_from_context:Dune_pkg.Solver_env.t option
  -> Dune_pkg.Solver_env.t

module Version_preference : sig
  val term : Dune_pkg.Version_preference.t option Term.t
end

module Per_context : sig
  type t =
    { lock_dir_path : Path.Source.t
    ; version_preference : Dune_pkg.Version_preference.t
    ; solver_env : Dune_pkg.Solver_env.t option
    ; repositories : Dune_pkg.Pkg_workspace.Repository.Name.t list
    ; context_common : Workspace.Context.Common.t
    ; repos :
        Dune_pkg.Pkg_workspace.Repository.t Dune_pkg.Pkg_workspace.Repository.Name.Map.t
    ; constraints : Dune_lang.Package_dependency.t list
    }

  val choose
    :  context_name_arg:Context_name.t option
    -> all_contexts_arg:bool
    -> version_preference_arg:Dune_pkg.Version_preference.t option
    -> t list Fiber.t
end

val get_repos
  :  Dune_pkg.Pkg_workspace.Repository.t Dune_pkg.Pkg_workspace.Repository.Name.Map.t
  -> repositories:Dune_pkg.Pkg_workspace.Repository.Name.t list
  -> update_opam_repositories:bool
  -> Dune_pkg.Opam_repo.t list Fiber.t

val find_local_packages : Dune_pkg.Local_package.t Package_name.Map.t Fiber.t

(** [pp_packages lock_dir] returns a list of pretty-printed packages
    occuring in [lock_dir]. *)
val pp_packages : Dune_pkg.Lock_dir.Pkg.t list -> 'a Pp.t
