open! Stdune
open! Import
open Memo.Build.O

module Initialization_state = struct
  type t =
    | Uninitialized of Path.t list
    | Initialized of { dune_file_watcher : Dune_file_watcher.t option }
end

(* Ideally this should be an [Fdecl], but there are currently two reasons why
   it's not:

   - We read the workspace file before we start the inotify watcher, so [init]
   is called after [t_ref] is used. This means we have to invalidate some
   entries of t when [init] is called and set up subscriptions.

   - There are tests that call [Scheduler.go] multiple times, therefore [init]
   gets called multiple times. Since they don't use file watcher, it shouldn't
   be a problem. *)
let t_ref = ref (Initialization_state.Uninitialized [])

(* CR-someday aalekseyev: For [watch_path] to work correctly we need to ensure
   that the parent directory of [path] exists. That is certainly not guaranteed
   by the [Fs_memo] API, so we should do something to make it more robust, but I
   believe that is masked by the fact that we usually (always?) look at the
   source directory before looking for files in that directory.

   It might seem that the [ENOENT] "fall back" trick used below can be extended
   to fall back all the way to the root, but it can't because subscribing to the
   root is not sufficient to receive events for creation of "root/a/b/c/d".
   (however, subscribing to "root/a/b/c" is sufficient for that) *)
let watch_path dune_file_watcher path =
  try Dune_file_watcher.add_watch dune_file_watcher path with
  | Unix.Unix_error (ENOENT, _, _) -> (
    (* If we're at the root of the workspace (or the unix root) then we can't
       get ENOENT because dune can't start without a workspace and unix root
       always exists, so this [_exn] can't raise (except if the user delets the
       workspace dir under our feet, in which case all bets are off). *)
    let containing_dir = Path.parent_exn path in
    (* If the file is absent, we need to wait for it to be created by watching
       the parent. We still try to add a watch for the file itself after that
       succeeds, in case the file was created already before we started watching
       its parent. *)
    Dune_file_watcher.add_watch dune_file_watcher containing_dir;
    try Dune_file_watcher.add_watch dune_file_watcher path with
    | Unix.Unix_error (ENOENT, _, _) -> ())

let watch_path_using_ref path =
  match !t_ref with
  | Initialized { dune_file_watcher = None } -> ()
  | Initialized { dune_file_watcher = Some watcher } -> watch_path watcher path
  | Uninitialized paths_to_watch ->
    t_ref := Uninitialized (path :: paths_to_watch)

(* Files and directories have non-overlapping sets of paths, so we can track
   them using the same memoization table. *)
let memo =
  Memo.create "fs_memo"
    ~input:(module Path)
    (fun path ->
      (* It may seem weird that we are adding a watch on every invalidation of
         the cell. This is OK because [add_watch] is idempotent, in the sense
         that we are not accumulating watches.

         In fact, if path disappears then we lose the watch and have to
         re-establish it, so doing it on every computation is sometimes
         necessary. *)
      watch_path_using_ref path;
      Memo.Build.return ())

let invalidate_path path =
  match Memo.Expert.previously_evaluated_cell memo path with
  | None -> Memo.Invalidation.empty
  | Some cell -> Memo.Cell.invalidate cell

let init ~dune_file_watcher =
  match !t_ref with
  | Initialized { dune_file_watcher = Some _ } ->
    Code_error.raise
      "Called [Fs_memo.init] a second time after a file watcher was already \
       set up "
      []
  | Initialized { dune_file_watcher = None } ->
    (* It would be nice to disallow this to simplify things, but there are tests
       that call [Scheduler.go] multiple times, therefore [init] gets called
       multiple times. Since they don't use the file watcher, it shouldn't be a
       problem. *)
    Memo.Invalidation.empty
  | Uninitialized accessed_paths ->
    let res =
      Memo.Invalidation.reduce (List.map accessed_paths ~f:invalidate_path)
    in
    t_ref := Initialized { dune_file_watcher };
    Option.iter dune_file_watcher ~f:(fun watcher ->
        List.iter accessed_paths ~f:(fun path -> watch_path watcher path));
    res

(* Declare a dependency on a path. Instead of calling [depend] directly, you
   should prefer using the helper function [declaring_dependency], because it
   calls [depend] and uses the corresponding path in the right order. *)
let depend path =
  if Path.is_in_build_dir path then
    Code_error.raise "Fs_memo.depend called on a build path" [];
  Memo.exec memo path

(* This does two things, in this order:

   - Declare a dependency on [path];

   - Sample the current value using the supplied function [f].

   If the order is reversed, the value can change after we sample it but before
   we register the dependency, which can result in memoizing a stale value. This
   scenario is purely hypothetical (at least for now) but it's nice to rule it
   out explicitly by doing things in the right order.

   Currently, we do not expose this low-level primitive. If you need it, perhaps
   you could add a higher-level primitive instead, such as [path_exists]? *)
let declaring_dependency path ~f =
  let+ () = depend path in
  f path

(* Assuming our file system watcher is any good, this and all subsequent
   untracked calls are safe. *)
let path_exists = declaring_dependency ~f:Path.Untracked.exists

(* CR-someday amokhov: Some call sites of [path_stat] care only about one field,
   such as [st_kind], and most of the file system events leave it as is. It may
   be useful to introduce a more precise variant of this function that will be
   invalidated less frequently. *)
let path_stat = declaring_dependency ~f:Path.Untracked.stat

(* CR-someday amokhov: It is unclear if we got the layers of abstraction right
   here. One could argue that caching is a higher-level concept compared to file
   watching, and we should expose this function from the [Cached_digest] module
   instead. For now, we keep it here because it seems nice to group all tracked
   file system access functions in one place, and exposing an uncached version
   of [file_digest] seems error-prone. We may need to rethink this decision. *)
let file_digest = declaring_dependency ~f:Cached_digest.source_or_external_file

let with_lexbuf_from_file path ~f =
  declaring_dependency path ~f:(fun path ->
      Io.Untracked.with_lexbuf_from_file path ~f)

let dir_contents_unsorted =
  declaring_dependency ~f:Path.Untracked.readdir_unsorted_with_kinds

(* When a file or directory is created or deleted, we need to also invalidate
   the parent directory, so that the [dir_contents] queries are re-executed. *)
let invalidate_path_and_its_parent path =
  Memo.Invalidation.combine (invalidate_path path)
    (match Path.parent path with
    | None -> Memo.Invalidation.empty
    | Some path -> invalidate_path path)

(* CR-someday amokhov: The way we currently treat file system events is simple
   and robust but doesn't take advantage of all the information we receive. Here
   are some ideas for future optimisation:

   - Don't invalidate [path_exists] queries on [File_changed] events.

   - If a [path_exists] query currently returns [true] and we receive a
   corresponding [File_deleted] event, we can change the query's result to
   [false] without rerunning the [Path.exists] function (and vice versa).

   - Similarly, the result of [dir_contents] queries can be updated without
   calling [Path.readdir_unsorted_with_kinds]: we know which file or directory
   should be added to or removed from the result. *)
let handle_fs_event ({ kind; path } : Dune_file_watcher.Fs_memo_event.t) :
    Memo.Invalidation.t =
  match kind with
  | File_changed -> invalidate_path path
  | Created
  | Deleted
  | Unknown ->
    invalidate_path_and_its_parent path
