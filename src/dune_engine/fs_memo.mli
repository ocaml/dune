open Import

(** [init] must be called at initialization. Returns the set of nodes that need
    to be invalidated because they were accessed before [init] was called. *)
val init : dune_file_watcher:Dune_file_watcher.t option -> Memo.Invalidation.t

(** Check if a source or external file exists and declare a dependency on it. *)
val file_exists : Path.Outside_build_dir.t -> bool Memo.t

(** Check if a source or external directory exists and declare a dependency on
    it. *)
val dir_exists : Path.Outside_build_dir.t -> bool Memo.t

val is_directory :
  Path.Outside_build_dir.t -> (bool, Unix_error.Detailed.t) result Memo.t

(** Call [Path.stat] on a path and declare a dependency on it. *)
val path_stat :
     Path.Outside_build_dir.t
  -> (Fs_cache.Reduced_stats.t, Unix_error.Detailed.t) result Memo.t

(** Like [path_stat] but extracts the [st_kind] field from the result. *)
val path_kind :
  Path.Outside_build_dir.t -> (File_kind.t, Unix_error.Detailed.t) result Memo.t

(** Digest the contents of a source or external file and declare a dependency on
    it. When [force_update = true], evict the file from all digest caches and
    force the recomputation of the digest. This can be useful if Dune made a
    change to the file and therefore knows that the cached digest is stale and
    is about to be invalidated by an incoming file-system event. By not using
    the cache in this situation, it's possible to avoid unnecessary restarts. *)
val file_digest :
     ?force_update:bool
  -> Path.Outside_build_dir.t
  -> Cached_digest.Digest_result.t Memo.t

(** Like [Io.Untracked.with_lexbuf_from_file] but declares a dependency on the
    path. *)
val with_lexbuf_from_file :
  Path.Outside_build_dir.t -> f:(Lexing.lexbuf -> 'a) -> 'a Memo.t

val file_contents : Path.Outside_build_dir.t -> string Memo.t

(** Read the contents of a source or external directory and declare a dependency
    on it. When [force_update = true], evict the directory from the file-system
    cache and force the recomputation of the result. This can be useful if Dune
    made a change to the directory and therefore knows that the cached contents
    is stale and is about to be invalidated by an incoming file-system event. By
    not using the cache in this situation, it's possible to avoid unnecessary
    restarts. *)
val dir_contents :
     ?force_update:bool
  -> Path.Outside_build_dir.t
  -> (Fs_cache.Dir_contents.t, Unix_error.Detailed.t) result Memo.t

(** Handle file system event. *)
val handle_fs_event : Dune_file_watcher.Fs_memo_event.t -> Memo.Invalidation.t
