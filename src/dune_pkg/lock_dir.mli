(** Frontend the lock directory format *)

open Import

module Pkg_info : sig
  type t =
    { name : Package_name.t
    ; version : Package_version.t
    ; dev : bool
    ; avoid : bool
    ; source : Source.t option
    ; extra_sources : (Path.Local.t * Source.t) list
    }

  val to_dyn : t -> Dyn.t
  val default_version : Package_version.t
  val variables : t -> OpamVariable.variable_contents Package_variable_name.Map.t
end

module Build_command : sig
  type t =
    | Action of Action.t
    | Dune (** pinned dune packages do not need to define a command *)

  val to_dyn : t -> Dyn.t
end

module Dependency : sig
  type t =
    { loc : Loc.t
    ; name : Package_name.t
    }

  val to_dyn : t -> Dyn.t
end

module Conditional_choice : sig
  (** A sequence of values, each conditional on an environment. *)
  type 'a t

  val empty : 'a t
  val singleton : Solver_env.t -> 'a -> 'a t

  (** Returns the first value whose associated environment is a subset of the
      specified environment. *)
  val choose_for_platform : 'a t -> platform:Solver_env.t -> 'a option
end

module Depexts : sig
  type t =
    { external_package_names : string list
    ; enabled_if : [ `Always | `Conditional of Slang.Blang.t ]
    }
end

module Pkg : sig
  type t =
    { build_command : Build_command.t Conditional_choice.t
    ; install_command : Action.t Conditional_choice.t
    ; depends : Dependency.t list Conditional_choice.t
    ; depexts : Depexts.t list
    ; info : Pkg_info.t
    ; exported_env : String_with_vars.t Action.Env_update.t list
    ; enabled_on_platforms : Solver_env.t list
    }

  val remove_locs : t -> t
  val equal : t -> t -> bool
  val hash : t -> int
  val to_dyn : t -> Dyn.t
  val files_dir : Package_name.t -> Package_version.t option -> lock_dir:Path.t -> Path.t
end

module Repositories : sig
  type t
end

module Packages : sig
  type t

  val to_pkg_list : t -> Pkg.t list
  val pkgs_on_platform_by_name : t -> platform:Solver_env.t -> Pkg.t Package_name.Map.t
end

type t = private
  { version : Syntax.Version.t
  ; dependency_hash : (Loc.t * Local_package.Dependency_hash.t) option
  ; packages : Packages.t
    (** It's guaranteed that this map will contain an entry for all dependencies
      of all packages in this map. That is, the set of packages is closed under
      the "depends on" relationship between packages. *)
  ; ocaml : (Loc.t * Package_name.t) option
  ; repos : Repositories.t
  ; expanded_solver_variable_bindings : Solver_stats.Expanded_variable_bindings.t
    (** Stores the solver variables that were evaluated while solving
      dependencies. Can be used to determine if a lockdir is compatible
      with a particular system. *)
  ; solved_for_platforms : Loc.t * Solver_env.t list
  }

val remove_locs : t -> t
val equal : t -> t -> bool
val to_dyn : t -> Dyn.t

(** [create_latest_version packages ~ocaml ~repos
    ~expanded_solver_variable_bindings] raises a [Code_error] if [packages] is
    not closed under the "depends on" relationship between packages. Every
    dependency of every package in [packages] must itself have a corresponding
    entry in [packages]. *)
val create_latest_version
  :  Pkg.t Package_name.Map.t
  -> local_packages:Local_package.For_solver.t list
  -> ocaml:(Loc.t * Package_name.t) option
  -> repos:Opam_repo.t list option
  -> expanded_solver_variable_bindings:Solver_stats.Expanded_variable_bindings.t
  -> solved_for_platform:Solver_env.t option
       (* TODO: make this non-optional when portable lockdirs becomes the default *)
  -> t

val default_path : Path.t Lazy.t

(** Returns the path to the lockdir that will be used to lock the
    given dev tool *)
val dev_tool_lock_dir_path : Dev_tool.t -> Path.t

module Metadata : Dune_sexp.Versioned_file.S with type data := unit

val metadata_filename : Filename.t

module Write_disk : sig
  type lock_dir := t
  type t

  val prepare
    :  portable_lock_dir:bool
    -> lock_dir_path:Path.t
    -> files:File_entry.t Package_version.Map.Multi.t Package_name.Map.t
    -> lock_dir
    -> t

  val commit : t -> unit
end

val read_disk : Path.t -> (t, User_message.t) result
val read_disk_exn : Path.t -> t

module Make_load (Io : sig
    include Monad.S

    val parallel_map : 'a list -> f:('a -> 'b t) -> 'b list t
    val readdir_with_kinds : Path.t -> (Filename.t * Unix.file_kind) list t
    val with_lexbuf_from_file : Path.t -> f:(Lexing.lexbuf -> 'a) -> 'a t
    val stats_kind : Path.t -> (File_kind.t, Unix_error.Detailed.t) result t
  end) : sig
  val load : Path.t -> (t, User_message.t) result Io.t
  val load_exn : Path.t -> t Io.t
end

(** [transitive_dependency_closure t ~platform names] returns the set of package names
    making up the transitive closure of dependencies of the set [names], or
    [Error (`Missing_packages missing_packages)] if if any element of [names]
    is not found in the lockdir. [missing_packages] is a subset of [names]
    not present in the lockdir. As a package's dependencies may vary between
    platforms, a description of the current platform must also be provided. *)
val transitive_dependency_closure
  :  t
  -> platform:Solver_env.t
  -> Package_name.Set.t
  -> (Package_name.Set.t, [ `Missing_packages of Package_name.Set.t ]) result

(** Attempt to download and compute checksums for packages that have source
    archive urls but no checksum. *)
val compute_missing_checksums : t -> pinned_packages:Package_name.Set.t -> t Fiber.t

(** Combine the platform-specific parts of a pair of lockdirs, throwing a code
    error if the lockdirs differ in a non-platform-specific way. *)
val merge_conditionals : t -> t -> t

(** Returns the packages contained in the solution on the given platform. If
    the lockdir does not contain a solution compatible with the given platform
    then a [User_error] is raised. *)
val packages_on_platform : t -> platform:Solver_env.t -> Pkg.t Package_name.Map.t
