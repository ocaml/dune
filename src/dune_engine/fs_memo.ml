open! Stdune
open! Import
open Memo.Build.O

type t = { dune_file_watcher : Dune_file_watcher.t option }

(* Ideally this should be an [Fdecl], but there are currently two reasons why
   it's not:

   - We read the workspace file before we start the inotify watcher, so [init]
   is called after [t_ref] is used. This is clearly not good (it means we never
   re-read the workspace file)

   - There are tests that call [Scheduler.go] multiple times, therefore [init]
   gets called multiple times. Since they don't use file watcher, it shouldn't
   be a problem. *)
let t_ref = ref { dune_file_watcher = None }

let init ~dune_file_watcher =
  match !t_ref.dune_file_watcher with
  | Some _ ->
    Code_error.raise
      "Called [Fs_memo.init] a second time after a file watcher was already \
       set up "
      []
  | None -> t_ref := { dune_file_watcher }

(* Files and directories have non-overlapping sets of paths, so we can track
   them using the same memoization table. *)
let memo =
  Memo.create "fs_memo"
    ~input:(module Path)
    (fun path ->
      let { dune_file_watcher } = !t_ref in
      Option.iter dune_file_watcher ~f:(fun dune_file_watcher ->
          try Dune_file_watcher.add_watch dune_file_watcher path with
          | Unix.Unix_error (ENOENT, _, _) -> (
            (* If the file is absent, we need to wait for it to be created by
               watching the parent. We still try to add a watch for the file
               itself after that succeeds, in case the file was created already
               before we started watching its parent. *)
            Dune_file_watcher.add_watch dune_file_watcher (Path.parent_exn path);
            try Dune_file_watcher.add_watch dune_file_watcher path with
            | Unix.Unix_error (ENOENT, _, _) -> ()));
      Memo.Build.return ())

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

let invalidate_path path =
  match Memo.Expert.previously_evaluated_cell memo path with
  | None -> Memo.Invalidation.empty
  | Some cell -> Memo.Cell.invalidate cell

(* When a file or directory is created or deleted, we need to also invalidate
   the parent directory, so that the [dir_contents] queries are re-executed. *)
let invalidate_path_and_its_parent path =
  Memo.Invalidation.combine (invalidate_path path)
    (match Path.parent path with
    | None -> Memo.Invalidation.empty
    | Some path -> invalidate_path path)

module Event = struct
  (* Here are some idealized assumptions about events:

     - If a file is renamed, we receive [Created] and [Deleted] events with
     corresponding paths.

     - If a directory is renamed then in addition to the [Created] and [Deleted]
     events for the directory itself, we receive events about all file and
     directory paths in the corresponding file tree.

     - Similarly, if a directory is deleted, we receive the [Deleted] event for
     the directory itself, as well as deletion events for all watched paths in
     the corresponding file tree.

     Very little of these assumptions currently hold. *)
  type kind =
    | Created
    | Deleted
    | File_changed
    | Unknown  (** Treated conservatively as any possible event. *)

  type t =
    { path : Path.t
    ; kind : kind
    }

  let create ~kind ~path =
    if Path.is_in_build_dir path then
      Code_error.raise "Fs_memo.Event.create called on a build path" [];
    { path; kind }

  (* CR-someday amokhov: The way we currently treat file system events is simple
     and robust but doesn't take advantage of all the information we receive.
     Here are some ideas for future optimisation:

     - Don't invalidate [path_exists] queries on [File_changed] events.

     - If a [path_exists] query currently returns [true] and we receive a
     corresponding [File_deleted] event, we can change the query's result to
     [false] without rerunning the [Path.exists] function (and vice versa).

     - Similarly, the result of [dir_contents] queries can be updated without
     calling [Path.readdir_unsorted_with_kinds]: we know which file or directory
     should be added to or removed from the result. *)
  let handle { kind; path } : Memo.Invalidation.t =
    match kind with
    | File_changed -> invalidate_path path
    | Created
    | Deleted
    | Unknown ->
      invalidate_path_and_its_parent path

  let path t = t.path

  let kind t = t.kind
end
