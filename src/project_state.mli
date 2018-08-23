(** Represents saved and current project digests
    and saved and current dependency graphs. *)
type t

(** Loads .project-db file from disk, or creates an empty db if it's absent. *)
val load : unit -> t

(** Returns current digest of a project.

    Digest is computed against project dependencies that were saved
    from the previous build, so in the case any dependencies change,
    the truly correct digest will be only calculated during the next build,
    when new dependencies are saved. This shouldn't be a correctness
    issue in practice.
*)
val get_current_digest
  : t
  -> Dune_project.Name.t
  -> projects:Dune_project.t Dune_project.Name.Table.t
  -> file_tree:File_tree.t
  -> Digest.t

(** Checks if current digest of a project differs from saved digest, if any. *)
val is_unclean
  :  t
  -> Dune_project.Name.t
  -> projects:Dune_project.t Dune_project.Name.Table.t
  -> file_tree:File_tree.t
  -> bool

(** Updates the current project dependency graph, adding a dependency from
    first parameter to second.
*)
val register_dependency
  :  t
  -> target:Dune_project.Name.t
  -> dependency:Dune_project.Name.t
  -> unit

(** Dumps current digests and dependencies to disk. *)
val finalize : t -> unit
