(** Represents saved and current project digests
    and saved and current dependency graphs. *)
type t

(** Loads .project-db file from disk, or creates an empty db if it's absent. *)
val load : unit -> t

(** For any project that turns out to be unclean (including dependencies
    of the project passed as argument), saved digest is dropped, as are
    all dependencies in the current project graph.
*)
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

(** Saves current digest and dependencies of project and all its dependencies. *)
val mark_clean : t -> Dune_project.Name.t -> unit

(** Dumps saved digests to disk. *)
val finalize : t -> unit
