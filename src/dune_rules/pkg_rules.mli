(** rules for packages built by dune *)

open Import
module Backing = Source_tree_file.Backing

val package_of_artifact_path : Path.Build.t -> Package.Name.t option

(** The same question for a directory named by its place in a context. *)
val package_of_dir_in_context : Path.Local.t -> Package.Name.t option

(** Nothing in a locked package is promoted. *)
val disable_promotion_in_package : dir:Path.Build.t -> Action.Full.t -> Action.Full.t

(** Where the source of a build path of a context is: the workspace, or the
    path itself inside a mounted package. *)
val source_location : Path.Build.t -> Path.t

(** A memo key for a package in a context. *)
module Context_and_package : Memo.Input with type t = Context_name.t * Package.Name.t

module Candidate : sig
  type t

  val name : t -> Package.Name.t
  val lock_pkg : t -> Dune_pkg.Lock_dir.Pkg.t
  val source_loc : t -> Loc.t
  val artifact_root : t -> Path.Build.t
end

(** The packages of the context's lock directory when it is active, otherwise
    nothing. *)
val candidates : Context_name.t -> Candidate.t Package.Name.Map.t Memo.t

(** Each candidate's locked dependencies on this platform, in lock order and
    without dune. Reading it touches no package source. *)
val dependencies : Context_name.t -> Package.Name.t list Package.Name.Map.t Memo.t

(** A locked package and how it is built: by its recipe, or by dune from the
    dune files of its mounted source. [None] when the package is not locked.
    Asking prepares the package's source. *)
val find_locked
  :  Context_name.t
  -> Package.Name.t
  -> (Candidate.t * [ `Opam | `Dune of Source_tree.Dir.t ]) option Memo.t

(** The directory a package's mount copies its source from, when its source is
    mounted. *)
val mount_backing : Context_name.t -> Package.Name.t -> Backing.t option Memo.t

(** A private copy of a mounted package's source, with its leading patches
    applied. Programs that run in the source, such as dune files in OCaml syntax,
    run there and not in the fetched source, which is shared between packages. *)
val prepared_source : Context_name.t -> Package.Name.t -> Path.Build.t

(** Selected lock packages use the context's [.pkg/<name>] subtree. Source
    snapshots are prepared independently in [_fetch]; opaque recipes install
    under [_install] within the package mount. Dev tools and their unshared
    dependencies retain their private package directories. *)

module Paths : sig
  type 'a t

  val of_root
    :  Package.Name.t
    -> root:'a
    -> source_dir:string
    -> install_dir:string
    -> relative:('a -> string -> 'a)
    -> 'a t

  val map_path : 'a t -> f:('a -> 'b) -> 'b t
  val target_dir : 'a t -> 'a
end

val candidate_paths : Candidate.t -> Path.Build.t Paths.t

module Dependency : sig
  type t

  val lib_root : t -> Path.t

  (** The package's dependency in a shared install layout at [prefix]. *)
  val native
    :  Path.t Paths.t
    -> prefix:Path.t
    -> variables:OpamVariable.variable_contents Package_variable_name.Map.t
    -> exported_env:String_with_vars.t Dune_lang.Action.Env_update.t list
    -> t

  val install_entries : t -> Path.t Install.Entry.Expanded.t list
end

module Layout : sig
  type t

  (** Materialize selected providers and register dependencies on installed
      files, without applying exports. Preserve the supplied order for applying
      exports. *)
  val materialize : Context_name.t -> Dependency.t list -> t Action_builder.t
end

(** The layout's standard paths, with the package exports applied in order. *)
val layout_env : Layout.t -> Env.t Action_builder.t

val setup_rules
  :  components:string list
  -> dir:Path.Build.t
  -> Context_name.t
  -> Build_config.Gen_rules.t Memo.t

(** Rules for [_build/_private/<context>/.dune-source/<package>]: the fetched
    source of a package with its lock directory files, extra sources and
    leading patches applied. Only packages that need this have rules. *)
val gen_prepared_source_rules
  :  Context_name.t
  -> dir:Path.Build.t
  -> string list
  -> Build_config.Gen_rules.t Memo.t

val gen_mounted_rule
  :  Context_name.t
  -> Candidate.t
  -> dependencies:(Package.Name.Set.t * Layout.t) Action_builder.t
  -> unit Memo.t

(** For each locked package whose dependency closure reaches the workspace, the
    workspace packages it reaches. *)
val workspace_dependencies
  :  Context_name.t
  -> Package.Name.Set.t Package.Name.Map.t Memo.t

val lock_dir_path : Context_name.t -> Path.t option Memo.t
val lock_dir_active : Context_name.t -> bool Memo.t
val ocaml_toolchain : Context_name.t -> Ocaml_toolchain.t Action_builder.t option Memo.t

(** What a rule running a tool of the locked compiler depends on: the package
    the lock directory names as the compiler, and its dependencies. A tool may
    need other files of that installation, a launcher its runtime for instance.
    Nothing when the context has no locked compiler. *)
val compiler_dep : Context_name.t -> unit Action_builder.t

(** [compiler_dep] when [prog] is installed by one of those packages. *)
val compiler_tool_dep : Context_name.t -> Path.t -> unit Action_builder.t

(** [packages] with the package installing the compiler and its dependencies.
    Their binaries are visible from every directory, whatever it declares. *)
val declared_packages
  :  Context_name.t
  -> Package.Name.Selection.t
  -> Package.Name.Set.t Memo.t

(** [which ~packages context program] looks [program] up among the binaries
    installed by opaque [packages] in the lock directory. Native declarations
    are resolved by [Artifacts] before this fallback. The caller supplies the
    packages visible to the requester, including any explicit dependencies.
    The package installing the compiler and its dependencies are always
    searched. *)
val which
  :  packages:Package.Name.Selection.t
  -> Context_name.t
  -> Filename.t
  -> Path.t option Memo.t

(** [env_for_packages ~packages context] is an env holding environment
    variables constructed from the [packages] in the lock directory. Only those
    packages are included, not their dependencies. [All] means the whole lock
    directory. Empty when the context has no lock directory. *)
val env_for_packages : packages:Package.Name.Selection.t -> Context_name.t -> Env.t Memo.t

(** [bin_paths_for_packages ~packages context] returns a list of bin
    directories of [packages] in the lock directory. [All] means the whole lock
    directory. Empty when the context has no lock directory. *)
val bin_paths_for_packages
  :  packages:Package.Name.Selection.t
  -> Context_name.t
  -> Path.t list Memo.t

(** [compiler_bin_paths context] are the bin directories of the compiler package
    of the lock directory of [context]. *)
val compiler_bin_paths : Context_name.t -> Path.t list Memo.t

val exported_env : Context_name.t -> Env.t Memo.t
val project_ocamlpath : Context_name.t -> Path.t list Memo.t
val dev_tool_ocamlpath : Dune_pkg.Dev_tool.t -> Path.t list Memo.t

(** The selected lock dependency closure and the action that builds its outputs.
    Inspecting the names does not build the package. *)
val find_package
  :  Context_name.t
  -> Package.Name.t
  -> (Package.Name.Set.t * unit Action_builder.t) option Memo.t

val dependency : Context_name.t -> Candidate.t -> Dependency.t Action_builder.t

val find_opaque
  :  Context_name.t
  -> Package.Name.t
  -> (Path.Build.t * Dependency.t Action_builder.t) option Memo.t

(* CR-someday Alizter: These prefixes would be [Path.Build.t] if not for
   the "toolchain" mechanism, which installs outside the build directory. *)

(** Installation prefixes and owning packages from the context's lock directory. *)
val package_prefixes : Context_name.t -> (Path.t * Package.Name.t) list Memo.t

val resolve_installed_file
  :  loc:Loc.t
  -> context_name:Context_name.t
  -> pkg_name:Package.Name.t
  -> section:Section.t
  -> file:Path.Local.t
  -> Path.t Action_builder.t

val dev_tool_env : Dune_pkg.Dev_tool.t -> Env.t Memo.t
val all_filtered_depexts : Context_name.t -> string list Memo.t

val setup_pkg_install_alias
  :  dir:Path.Build.t
  -> Context_name.t
  -> Build_config.Gen_rules.t
