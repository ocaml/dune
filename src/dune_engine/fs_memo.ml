open! Stdune
open! Import
open Memo.Build.O

(* [Fs_memo] can be in three possible states:

   - It starts in [Waiting_for_file_watcher], accumulating [paths_to_watch] to
   pass them to the file watcher once it has been initialised.

   - If the file watcher turns out to be missing, the state [No_file_watcher] is
   used to indicate that there is no need to accumulate [paths_to_watch].

   - [File_watcher] holds [Dune_file_watcher.t] once it has been initialised and
   all previously collected [paths_to_watch] have been passed to it. *)
type state =
  | Waiting_for_file_watcher of { paths_to_watch : Path.t list }
  | No_file_watcher
  | File_watcher of Dune_file_watcher.t

(* Ideally this should be an [Fdecl], but there are currently two reasons why
   it's not:

   - We read the workspace file before the file watcher has been initialised, so
   [init] is called after [state] is used. Hence, we accumulate [paths_to_watch]
   to invalidate and start watching them when [init] is called.

   - There are tests that call [Scheduler.go] multiple times, therefore [init]
   gets called multiple times. Since these tests don't use the file watcher, it
   shouldn't be a problem. *)
let state = ref (Waiting_for_file_watcher { paths_to_watch = [] })

(* CR-someday aalekseyev: For [watch_path] to work correctly we need to ensure
   that the parent directory of [path] exists. That is certainly not guaranteed
   by the [Fs_memo] API, so we should do something to make it more robust, but I
   believe that is masked by the fact that we usually (always?) look at the
   source directory before looking for files in that directory.

   It might seem that the [`Does_not_exist] "fall back to the containing dir"
   trick used below can be extended to fall back all the way to the root, but it
   can't be because watching the root is not sufficient to receive events for
   creation of "root/a/b/c/d" -- for that we need to watch "root/a/b/c". *)
let watch_path watcher path =
  match Dune_file_watcher.add_watch watcher path with
  | Ok () -> ()
  | Error `Does_not_exist -> (
    (* If we're at the root of the workspace (or the Unix root) then we can't
       get [`Does_not_exist] because Dune can't start without a workspace and
       the Unix root always exists. Hence, the [_exn] below can't raise, except
       if the user deletes the workspace directory under our feet, in which case
       all bets are off. *)
    let containing_dir = Path.parent_exn path in
    (* If the [path] is absent, we need to wait for it to be created by watching
       the parent. We still try to add a watch for the [path] itself after that
       succeeds, in case the [path] was created already before we started
       watching its parent. *)
    (match Dune_file_watcher.add_watch watcher containing_dir with
    | Ok () -> ()
    | Error `Does_not_exist ->
      Log.info
        [ Pp.textf "Attempted to add watch to non-existent directory %s."
            (Path.to_string containing_dir)
        ]);
    match Dune_file_watcher.add_watch watcher path with
    | Error `Does_not_exist
    | Ok () ->
      ())

let watch_or_record_path path =
  match !state with
  | Waiting_for_file_watcher { paths_to_watch } ->
    state :=
      Waiting_for_file_watcher { paths_to_watch = path :: paths_to_watch }
  | No_file_watcher -> ()
  | File_watcher dune_file_watcher -> watch_path dune_file_watcher path

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
      watch_or_record_path path;
      Memo.Build.return ())

module Update_all = Monoid.Function (Path) (Fs_cache.Update_result)

let update_all : Path.t -> Fs_cache.Update_result.t =
  let update t path =
    let result = Fs_cache.update t path in
    if !Clflags.debug_fs_cache then
      Console.print_user_message
        (User_message.make
           [ Pp.hbox
               (Pp.textf "Updating %s cache for %S: %s" (Fs_cache.Debug.name t)
                  (Path.to_string path)
                  (Dyn.to_string (Fs_cache.Update_result.to_dyn result)))
           ]);
    result
  in
  Update_all.reduce
    [ update Fs_cache.Untracked.path_stat
    ; update Fs_cache.Untracked.file_digest
    ; update Fs_cache.Untracked.dir_contents
    ]

(* CR-someday amokhov: We use the same Memo table [memo] for tracking different
   file-system operations. This saves us some memory, but leads to recomputing
   more memoized functions than necessary. We could create a separate Memo table
   for each [Fs_cache] operation, or even better use [Fs_cache] tables in Memo
   directory, perhaps via [Memo.create_with_store]. *)
let invalidate_path path =
  match update_all path with
  | Skipped
  | Updated { changed = false } ->
    Memo.Invalidation.empty
  | Updated { changed = true } ->
    Memo.Cell.invalidate (Memo.cell memo path) ~reason:(Path_changed path)

let init ~dune_file_watcher =
  match !state with
  | File_watcher _ ->
    Code_error.raise
      "Called [Fs_memo.init] a second time after a file watcher was already \
       set up"
      []
  | No_file_watcher ->
    (* It would be nice to disallow this branch to simplify things, but there
       are tests that call [Scheduler.go] multiple times, therefore [init] gets
       called multiple times with [dune_file_watcher = None]. Since they don't
       use the file watcher, it shouldn't be a problem. *)
    if Option.is_some dune_file_watcher then
      Code_error.raise
        "Called [Fs_memo.init] a second time after a file watcher was already \
         declared as missing"
        [];
    Memo.Invalidation.empty
  | Waiting_for_file_watcher { paths_to_watch } ->
    (match dune_file_watcher with
    | None -> state := No_file_watcher
    | Some watcher ->
      state := File_watcher watcher;
      List.iter paths_to_watch ~f:(fun path -> watch_path watcher path));
    Memo.Invalidation.reduce (List.map paths_to_watch ~f:invalidate_path)

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
   you could add a higher-level primitive instead, such as [file_exists]? *)
let declaring_dependency path ~f =
  let+ () = depend path in
  f path

let path_stat = declaring_dependency ~f:Fs_cache.(read Untracked.path_stat)

(* We currently implement [file_exists] and [dir_exists] functions by calling
   [Fs_cache.path_stat] instead of creating separate [Fs_cache] primitives. Here
   are some reasons for doing this:

   - Semantically, this is equivalent because [Path.exists] is also implemented
   by checking that the [stat] call succeeds (see [caml_sys_file_exists]).

   - [Fs_cache.path_stat] doesn't change too often because the resulting record
   [Reduced_stats] doesn't include frequently changing fields like [mtime].
   Technically, it can still change while [file_exists] remains [false], for
   example, if the [st_kind] field changes from [S_DIR] to [S_LNK]. In this case
   Dune will restart unnecessarily, but such changes are rare, so we don't care.

   - Reusing [Fs_cache.path_stat] helps us to reduce the number of [stat] calls,
   i.e. we can do just one call to answer both [path_stat] and [file_exists].

   - Having a smaller number of primitives in [Fs_cache] is better because it
   makes it easier to think about reasons for restarting the current build and
   avoids unnecessary cache tables in [Fs_cache]. *)
let path_kind =
  declaring_dependency
    ~f:
      Fs_cache.(
        fun path ->
          read Untracked.path_stat path |> function
          (* If the set of ignored fields below changes, it may be necessary to
             introduce a separate [Fs_cache.path_kind] primitive to avoid
             unnecessary restarts. *)
          | Ok { st_dev = _; st_ino = _; st_kind } -> Ok st_kind
          | Error _ as error -> error)

let file_exists path =
  path_kind path >>| function
  | Ok kind -> File_kind.equal kind S_REG
  | Error (_ : Unix_error.Detailed.t) -> false

let dir_exists path =
  path_kind path >>| function
  | Ok kind -> File_kind.equal kind S_DIR
  | Error (_ : Unix_error.Detailed.t) -> false

(* CR-someday amokhov: It is unclear if we got the layers of abstraction right
   here. One could argue that caching is a higher-level concept compared to file
   watching, and we should expose this function from the [Cached_digest] module
   instead. For now, we keep it here because it seems nice to group all tracked
   file system access functions in one place, and exposing an uncached version
   of [file_digest] seems error-prone. We may need to rethink this decision. *)
let file_digest ?(force_update = false) path =
  if force_update then (
    Cached_digest.Untracked.invalidate_cached_timestamp path;
    Fs_cache.evict Fs_cache.Untracked.file_digest path
  );
  declaring_dependency path ~f:Fs_cache.(read Untracked.file_digest)

let dir_contents ?(force_update = false) path =
  if force_update then Fs_cache.evict Fs_cache.Untracked.dir_contents path;
  declaring_dependency path ~f:Fs_cache.(read Untracked.dir_contents)

(* CR-someday amokhov: For now, we do not cache the result of this operation
   because the result's type depends on [f]. There are only two call sites of
   this function, so perhaps we could just replace this more general function
   with two simpler one that can be cached independently. *)
let with_lexbuf_from_file path ~f =
  declaring_dependency path ~f:(fun path ->
      (* This is a bit of a hack. By reading [file_digest], we cause the [path]
         to be recorded in the [Fs_cache.Untracked.file_digest], so the build
         will be restarted if the digest changes. *)
      ignore Fs_cache.(read Untracked.file_digest path);
      Io.Untracked.with_lexbuf_from_file path ~f)

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

   - Don't invalidate [file_exists] queries on [File_changed] events.

   - If [file_exists] currently returns [true] and we receive a corresponding
   [Deleted] event, we can change the result to [false] without rerunning the
   [Path.exists] function. Similarly for the case where [file_exists] is [false]
   and we receive a corresponding [Created] event.

   - Finally, the result of [dir_contents] queries can be updated without
   calling [Path.Untracked.readdir_unsorted_with_kinds]: we know which file or
   directory should be added to or removed from the result. *)
let handle_fs_event ({ kind; path } : Dune_file_watcher.Fs_memo_event.t) :
    Memo.Invalidation.t =
  match kind with
  | File_changed -> invalidate_path path
  | Created
  | Deleted
  | Unknown ->
    invalidate_path_and_its_parent path
