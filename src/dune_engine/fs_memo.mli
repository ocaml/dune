open Import

module Dir_contents : sig
  type t

  (** The list of file names with kinds. *)
  val to_list : t -> (Filename.t * File_kind.t) list

  val iter : t -> f:(Filename.t -> File_kind.t -> unit) -> unit
end

module Reduced_stats : sig
  type t =
    { st_dev : int
    ; st_ino : int
    ; st_kind : Unix.file_kind
    }
end

(** [init] must be called at initialization. Returns the set of nodes that need
      to be invalidated because they were accessed before [init] was called. *)
val init : dune_file_watcher:Dune_scheduler.File_watcher.t option -> Memo.Invalidation.t

val handle_fs_event : Dune_scheduler.Event.Fs_memo_event.t -> Memo.Invalidation.t

(** Check if a source or external file exists and declare a dependency on it. *)
val file_exists : Path.Outside_build_dir.t -> bool Memo.t

(** Check if a source or external directory exists and declare a dependency on
    it. *)
val dir_exists : Path.Outside_build_dir.t -> bool Memo.t

val is_directory : Path.Outside_build_dir.t -> (bool, Unix_error.Detailed.t) result Memo.t

(** Call [Path.stat] on a path and declare a dependency on it. *)
val path_stat
  :  Path.Outside_build_dir.t
  -> (Reduced_stats.t, Unix_error.Detailed.t) result Memo.t

(** Like [path_stat] but extracts the [st_kind] field from the result. *)
val path_kind
  :  Path.Outside_build_dir.t
  -> (File_kind.t, Unix_error.Detailed.t) result Memo.t

(** Track a promoted source file without digesting it. Unlike
    direct source readers, this does not mark the file as directly read by the
    build, so Dune's self-generated promotion events can still be suppressed
    when the promoted file is up to date. *)
val track_promoted_file_changes : Path.Source.t -> unit Memo.t

(** Track a source file that is copied into the build directory. File-change
    events for the source may be ignored when all registered build-directory
    copies already have the same contents and executable bit, and there are no
    direct source readers for the path. *)
val track_source_file_copy : src:Path.Source.t -> dst:Path.Build.t -> unit Memo.t

module Source_file_copy_cache : sig
  val find
    :  src:Path.Source.t
    -> dst:Path.Build.t
    -> src_stats:Stat.t
    -> dst_stats:Stat.t
    -> Digest.t option

  val store
    :  src:Path.Source.t
    -> dst:Path.Build.t
    -> src_stats:Stat.t
    -> dst_stats:Stat.t
    -> dst_digest:Digest.t
    -> unit
end

(** Check whether a source-copy target is already up to date. This does not
    register a dependency. *)
val source_file_copy_is_up_to_date : src:Path.Source.t -> dst:Path.Build.t -> bool

(** Digest an external file and declare a dependency on it, raising a user
    error if it is unavailable. *)
val file_digest_exn
  :  loc:(unit -> Loc.t option Memo.t)
  -> Path.External.t
  -> Digest.t Memo.t

(** Like [Io.Untracked.with_lexbuf_from_file] but declares a dependency on the
    path. *)
val with_lexbuf_from_file
  :  Path.Outside_build_dir.t
  -> f:(Lexing.lexbuf -> 'a)
  -> 'a Memo.t

val file_contents : Path.Outside_build_dir.t -> string Memo.t

(** Read the contents of a source or external directory and declare a dependency
    on it. When [force_update = true], evict the directory from the file-system
    cache and force the recomputation of the result. This can be useful if Dune
    made a change to the directory and therefore knows that the cached contents
    is stale and is about to be invalidated by an incoming file-system event. By
    not using the cache in this situation, it's possible to avoid unnecessary
    restarts. *)
val dir_contents
  :  ?force_update:bool
  -> Path.Outside_build_dir.t
  -> (Dir_contents.t, Unix_error.Detailed.t) result Memo.t

module Untracked : sig
  val path_stat
    :  Path.Outside_build_dir.t
    -> (Reduced_stats.t, Unix_error.Detailed.t) result
end

(** {1 Managing the cache} *)

(** Invalidate all cached [stat] values. This causes all subsequent calls to
    [build_file] or [source_or_external_file] to incur additional [stat] calls. *)
val invalidate_cached_timestamps : unit -> unit

module Debug : sig
  val dump_digest_db : Path.t list -> Dyn.t
  val check_digest_db : Path.t list -> Dyn.t
end
