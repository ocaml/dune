open! Stdune
open Import

(** [init] must be called at initialization. Returns the set of nodes that need
    to be invalidated because they were accessed before [init] was called. *)
val init : dune_file_watcher:Dune_file_watcher.t option -> Memo.Invalidation.t

(** All functions in this module raise a code error when given a path in the
    build directory. *)

(* CR-someday amokhov: Note that currently the scheduler calls [handle] only for
   source paths, because we don't watch external directories. We should try to
   implement at least a partial support for watching external paths. *)

(** Check if a source or external path exists and declare a dependency on it. *)
val path_exists : Path.t -> bool Memo.Build.t

(** Call [Path.stat] on a path and declare a dependency on it. *)
val path_stat : Path.t -> (Unix.stats, Unix.error) result Memo.Build.t

(** Digest the contents of a source or external file and declare a dependency on
    it. *)
val file_digest : Path.t -> Digest.t Memo.Build.t

(** Like [Io.Untracked.with_lexbuf_from_file] but declares a dependency on the
    path. *)
val with_lexbuf_from_file : Path.t -> f:(Lexing.lexbuf -> 'a) -> 'a Memo.Build.t

(* CR-someday amokhov: Avoid restarting the build when file access functions
   like [path_exists] or [dir_contents_unsorted] evaluate to the same result
   after handling a file system event. This can be supported by adding a layer
   of memoization nodes with "eager cutoff". In this layer, [invalidate] calls
   will force eager evaluation of the memoized functions to check if the cutoff
   condition is satisfied and, if yes, the build will not need to be restarted.
   Once this is supported, we can introduce a variant of [dir_contents_unsorted]
   that sorts the result and is included into the "eager cutoff" layer, so that
   we can avoid triggering rebuilds due to the non-determinism in the order in
   which the file system lists directory entries. *)

(** List the contents of a source or external directory and declare a dependency
    on it. The result is unsorted and includes both name and kind of each entry. *)
val dir_contents_unsorted :
  Path.t -> ((string * Unix.file_kind) list, Unix.error) result Memo.Build.t

(** Handle file system event. *)
val handle_fs_event : Dune_file_watcher.Fs_memo_event.t -> Memo.Invalidation.t
