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
end

val root : unit -> Dir.t Memo.t

(** The source view used by rule generation. Its directories and files retain
    either workspace or build-target ownership; callers use this API rather than
    dispatching on generic paths. *)
module Rules : sig
  module File : sig
    type contents =
      { contents : string
      ; executable : bool
      ; dependencies : Path.t list
      }

    type materialization =
      | Copy of Path.t
      | Write of contents

    type t

    val source_path : t -> Source_path.t
    val path : t -> Path.t
    val backing_path_opt : t -> Path.t option Memo.t
    val backing_path : t -> Path.t Memo.t
    val relative : t -> Loc.t -> string -> t
    val materialization : t -> materialization option Memo.t
    val read_with_perm : t -> contents option Memo.t
    val read : t -> string option Memo.t
    val equal : t -> t -> bool
    val diagnostic_name : t -> string
    val include_context : t -> t Include_stanza.context
  end

  module Dir : sig
    type t
    type sub_dir

    val source : Dir.t -> t
    val source_path : t -> Source_path.t

    (** The logical directory used by source semantics. *)
    val path : t -> Path.t

    (** The directory from which source contents are enumerated. *)
    val backing_path : t -> Path.t

    val file : t -> Filename.t -> File.t
    val relative_dir : t -> Path.Local.t
    val filenames : t -> Filename.Array.Set.t
    val sub_dirs : t -> sub_dir Filename.Array.Map.t
    val sub_dir_names : t -> Filename.Array.Set.t
    val sub_dir_as_t : sub_dir -> t Memo.t
    val find_dir : t -> Path.Local.t -> t option Memo.t
    val status : t -> Source_dir_status.t
    val dune_file : t -> Dune_file.t option
    val project : t -> Dune_project.t
    val to_dyn : t -> Dyn.t

    module Make_map_reduce (M : Memo.S) (Outcome : Monoid) : sig
      val map_reduce
        :  t
        -> traverse:Source_dir_status.Set.t
        -> trace_event_name:string
        -> f:(t -> Outcome.t M.t)
        -> Outcome.t M.t
    end
  end

  module Mounted : sig
    module Layer : sig
      type t

      val directory : logical_root:Path.Build.t -> backing_root:Path.Build.t -> t
      val file : logical:Path.Build.t -> backing:Path.Build.t -> t

      val contents
        :  logical:Path.Build.t
        -> contents:string
        -> executable:bool
        -> dependencies:Path.t list
        -> t

      val delete : logical:Path.Build.t -> t
    end

    type t

    val read_file
      :  logical:Path.Build.t
      -> layers:Layer.t list
      -> File.contents option Memo.t

    (** Load a logical mounted tree from ordered immutable input layers. Later
        layers replace earlier files at the same logical path. *)
    val load : logical_root:Path.Build.t -> layers:Layer.t list -> t Memo.t

    (** The logical mounted root. *)
    val source_root : t -> Path.Build.t

    val root : t -> Dir.t
    val projects : t -> (Dune_project.t * Dir.t) list
  end
end

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

val files_of : Path.Source.t -> Path.Source.Set.t Memo.t

(** [true] iff the path is a vendored directory *)
val is_vendored : Path.Source.t -> bool Memo.t

(** [nearest_vcs t fn] returns the version control system with the longest root
    path that is an ancestor of [fn]. *)
val nearest_vcs : Path.Source.t -> Vcs.t option Memo.t
