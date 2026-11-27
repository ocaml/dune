(** Dune representation of the source tree *)

open Import

module Dir : sig
  type t

  val path : t -> Path.Source.t
  val filenames : t -> Filename.Array.Set.t

  type sub_dir

  val sub_dirs : t -> sub_dir Filename.Array.Map.t
  val sub_dir_as_t : sub_dir -> t Memo.t

  module Make_map_reduce (M : Memo.S) (Outcome : Monoid) : sig
    (** Traverse sub-directories recursively, pass them to [f] and combine
        intermediate results into a single one via [M.combine]. *)
    val map_reduce
      :  t
      -> traverse:Source_dir_status.Set.t
      -> trace_event_name:string
      -> f:(t -> Outcome.t M.t)
      -> Outcome.t M.t
  end

  val sub_dir_names : t -> Filename.Array.Set.t
  val status : t -> Source_dir_status.t

  (** Return the contents of the dune (or jbuild) file in this directory *)
  val dune_file : t -> Dune_file.t option

  (** Return the project this directory is part of *)
  val project : t -> Dune_project.t

  val to_dyn : t -> Dyn.t
  val source_path : t -> Path.Local.t

  (** The logical directory used by source semantics. *)
  val logical_path : t -> Path.t

  val file : t -> Filename.t -> Source_tree_file.File.t
  val find_dir : t -> Path.Local.t -> t option Memo.t
end

val root : unit -> Dir.t Memo.t

(** Load the tree at [logical_root] from its external or built backing. *)
val load_mounted
  :  loc:Loc.t
  -> logical_root:Path.Build.t
  -> backing_root:Source_tree_file.Backing.t
  -> Dir.t Memo.t

module Make_map_reduce_with_progress (M : Memo.S) (Outcome : Monoid) : sig
  (** Traverse starting from the root and report progress in the status line *)
  val map_reduce
    :  traverse:Source_dir_status.Set.t
    -> trace_event_name:string
    -> f:(Dir.t -> Outcome.t M.t)
    -> Outcome.t M.t
end

val find_dir : Path.Source.t -> Dir.t option Memo.t

(** [find_excluded_ancestor path] is the ancestor of [path] that was excluded by
    a dirs stanza, if any. *)
val find_excluded_ancestor : Path.Source.t -> (Path.Source.t * Loc.t) option Memo.t

(** [nearest_dir t fn] returns the directory with the longest path that is an
    ancestor of [fn]. *)
val nearest_dir : Path.Source.t -> Dir.t Memo.t

(** [true] iff the path is a vendored directory *)
val is_vendored : Path.Source.t -> bool Memo.t

(** [nearest_vcs t fn] returns the version control system with the longest root
    path that is an ancestor of [fn]. *)
val nearest_vcs : Path.Source.t -> Vcs.t option Memo.t
