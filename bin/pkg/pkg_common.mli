open Import

val context_term : doc:string -> Context_name.t option Term.t

val solver_env_variables
  :  solver_sys_vars_from_context:Dune_pkg.Solver_env.Variable.Sys.Bindings.t option
  -> sys_bindings_from_current_system:Dune_pkg.Solver_env.Variable.Sys.Bindings.t
  -> Dune_pkg.Solver_env.Variable.Sys.Bindings.t

module Version_preference : sig
  val term : Dune_pkg.Version_preference.t option Term.t
end

module Per_context : sig
  type t =
    { lock_dir_path : Path.Source.t
    ; version_preference : Dune_pkg.Version_preference.t
    ; solver_sys_vars : Dune_pkg.Solver_env.Variable.Sys.Bindings.t option
    ; repositories : Dune_pkg.Pkg_workspace.Repository.Name.t list
    ; context_common : Workspace.Context.Common.t
    ; repos :
        Dune_pkg.Pkg_workspace.Repository.t Dune_pkg.Pkg_workspace.Repository.Name.Map.t
    }

  val choose
    :  context_name_arg:Context_name.t option
    -> all_contexts_arg:bool
    -> version_preference_arg:Dune_pkg.Version_preference.t option
    -> t list Fiber.t
end

val get_repos
  :  Dune_pkg.Pkg_workspace.Repository.t Dune_pkg.Pkg_workspace.Repository.Name.Map.t
  -> opam_repository_path:Path.t option
  -> opam_repository_url:OpamUrl.t option
  -> repositories:Dune_pkg.Pkg_workspace.Repository.Name.t list
  -> update_opam_repositories:bool
  -> Dune_pkg.Opam_repo.t list Fiber.t

val find_local_packages : Dune_pkg.Local_package.t Package_name.Map.t Fiber.t

module Opam_repository_path : sig
  val term : Path.t option Term.t
end

module Opam_repository_url : sig
  val term : OpamUrl.t option Term.t
end

(** [pp_packages lock_dir] returns a list of pretty-printed packages
    occuring in [lock_dir]. *)
val pp_packages : Dune_pkg.Lock_dir.Pkg.t list -> 'a Pp.t
