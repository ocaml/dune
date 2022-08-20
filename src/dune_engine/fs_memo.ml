open Import
open Memo.O

(* Watching and invalidating paths. *)
module Watcher : sig
  val init : dune_file_watcher:Dune_file_watcher.t option -> Memo.Invalidation.t

  (* Watch a path. You should call this function *before* accessing the file
     system to prevent possible races.

     If [try_to_watch_via_parent = true], the watcher will attempt to watch the
     path by watching its parent directory, falling back to watching the path
     directly if there is no parent. This is an optimisation that allows us to
     reduce the number of watched paths: typically, the number of directories is
     a lot smaller than the number of files. *)
  val watch :
    try_to_watch_via_parent:bool -> Path.Outside_build_dir.t -> unit Memo.t

  (* Invalidate a path after receiving an event from the file watcher. *)
  val invalidate : Path.Outside_build_dir.t -> Memo.Invalidation.t
end = struct
  (* A record of a call to [watch] made while the file watcher was missing. *)
  type watch_record =
    { accessed_path : Path.Outside_build_dir.t
    ; path_to_watch : Path.Outside_build_dir.t
    }

  (* CR-someday amokhov: We should try to simplify the initialisation of the
     file watcher. We probably no longer need this complexity because we no
     longer use an external process for file-watching. *)

  (* This module can be in three possible states:

     - It starts in [Waiting_for_file_watcher], accumulating [watched_record]s
     to pass them to the file watcher once it has been initialised.

     - If the file watcher turns out to be missing, the state [No_file_watcher]
     is used to indicate that there is no need to accumulate [watched_record]s.

     - [File_watcher] holds [Dune_file_watcher.t] once it has been initialised
     and all previously collected [watched_record]s have been passed to it. *)
  type state =
    | Waiting_for_file_watcher of watch_record list
    | No_file_watcher
    | File_watcher of Dune_file_watcher.t

  (* Ideally this should be an [Fdecl] instead of a mutable reference, but there
     are currently two reasons why it's not:

     - We read the workspace file before the file watcher has been initialised,
     so [init] is called after [state] is used. We accumulate [watch_record]s to
     process them when [init] is finally called.

     - There are tests that call [Scheduler.go] multiple times, therefore [init]
     can be called multiple times. Since these tests don't use the file watcher,
     it shouldn't be a problem. *)
  let state = ref (Waiting_for_file_watcher [])

  (* CR-someday aalekseyev: For [watch_path] to work correctly we need to ensure
     that the parent directory of [path] exists. That's certainly not guaranteed
     by the [Fs_memo] API, so we should do something to make it more robust, but
     I believe that is masked by the fact that we usually (always?) look at the
     source directory before looking for files in that directory.

     It might seem that the [`Does_not_exist] "fall back to the containing dir"
     trick used below can be extended to fall back all the way to the root, but
     it can't be because watching the root is not sufficient to receive events
     for creation of "root/a/b/c" -- for that we need to watch "root/a/b". *)
  let watch_path watcher path =
    match Dune_file_watcher.add_watch watcher path with
    | Ok () -> ()
    | Error `Does_not_exist -> (
      (* If we're at the root of the workspace (or the Unix root) then we can't
         get [`Does_not_exist] because Dune can't start without a workspace and
         the Unix root always exists. Hence, the [_exn] below can't raise,
         except if the user deletes the workspace directory under our feet, in
         which case all bets are off. *)
      let containing_dir = Path.parent_exn path in
      (* If the [path] is absent, we need to wait for it to be created by
         watching the parent. We still try to add a watch for the [path] itself
         after that succeeds, in case the [path] was created already before we
         started watching its parent. *)
      (match Dune_file_watcher.add_watch watcher containing_dir with
      | Ok () -> ()
      | Error `Does_not_exist ->
        Log.info
          [ Pp.textf "Attempted to add watch to non-existent directory %s."
              (Path.to_string containing_dir)
          ]);
      match Dune_file_watcher.add_watch watcher path with
      | Error `Does_not_exist | Ok () -> ())

  let watch_or_record_path ~accessed_path ~path_to_watch =
    match !state with
    | Waiting_for_file_watcher watch_records ->
      state :=
        Waiting_for_file_watcher
          ({ accessed_path; path_to_watch } :: watch_records)
    | No_file_watcher -> ()
    | File_watcher dune_file_watcher ->
      let path_to_watch = Path.outside_build_dir path_to_watch in
      watch_path dune_file_watcher path_to_watch

  (* This comment applies to both memoization tables below.

     It may seem weird that we are adding a watch on every invalidation of the
     cell. This is OK because [Dune_file_watcher.add_watch] is idempotent, in
     the sense that we are not accumulating watches. In fact, if a path
     disappears then we lose the watch and have to re-establish it, so doing it
     on every computation is sometimes necessary. *)
  let memo_for_watching_directly =
    Memo.create "fs_memo_for_watching_directly"
      ~input:(module Path.Outside_build_dir)
      (fun accessed_path ->
        watch_or_record_path ~accessed_path ~path_to_watch:accessed_path;
        Memo.return ())

  let memo_for_watching_via_parent =
    Memo.create "fs_memo_for_watching_via_parent"
      ~input:(module Path.Outside_build_dir)
      (fun accessed_path ->
        let path_to_watch =
          Option.value
            (Path.Outside_build_dir.parent accessed_path)
            ~default:accessed_path
        in
        watch_or_record_path ~accessed_path ~path_to_watch;
        Memo.return ())

  let watch ~try_to_watch_via_parent path =
    match try_to_watch_via_parent with
    | false -> Memo.exec memo_for_watching_directly path
    | true -> Memo.exec memo_for_watching_via_parent path

  module Update_all =
    Monoid.Function (Path.Outside_build_dir) (Fs_cache.Update_result)

  let update_all : Path.Outside_build_dir.t -> Fs_cache.Update_result.t =
    let update t path =
      let result = Fs_cache.update t path in
      if !Clflags.debug_fs_cache then
        Console.print_user_message
          (User_message.make
             [ Pp.hbox
                 (Pp.textf "Updating %s cache for %S: %s"
                    (Fs_cache.Debug.name t)
                    (Path.Outside_build_dir.to_string path)
                    (Dyn.to_string (Fs_cache.Update_result.to_dyn result)))
             ]);
      result
    in
    fun p ->
      let all =
        [ update Fs_cache.Untracked.path_stat
        ; update Fs_cache.Untracked.file_digest
        ; update Fs_cache.Untracked.dir_contents
        ]
      in
      Update_all.reduce all p

  (* CR-someday amokhov: We share Memo tables for tracking different file-system
     operations. This saves some memory, but leads to recomputing more memoized
     functions than necessary. We can use a separate table for each [Fs_cache]
     operation, or even better use [Fs_cache] tables in Memo directory, e.g. via
     [Memo.create_with_store]. *)
  let invalidate path =
    match update_all path with
    | Skipped | Updated { changed = false } -> Memo.Invalidation.empty
    | Updated { changed = true } ->
      let reason : Memo.Invalidation.Reason.t =
        Path_changed (Path.outside_build_dir path)
      in
      Memo.Invalidation.combine
        (Memo.Cell.invalidate
           (Memo.cell memo_for_watching_directly path)
           ~reason)
        (Memo.Cell.invalidate
           (Memo.cell memo_for_watching_via_parent path)
           ~reason)

  let init ~dune_file_watcher =
    match !state with
    | File_watcher _ ->
      Code_error.raise
        "Called [Fs_memo.init] a second time after a file watcher was already \
         set up"
        []
    | No_file_watcher ->
      (* It would be nice to disallow this branch to simplify things, but there
         are tests that call [Scheduler.go] multiple times, therefore [init]
         gets called multiple times with [dune_file_watcher = None]. Since they
         don't use the file watcher, it shouldn't be a problem. *)
      if Option.is_some dune_file_watcher then
        Code_error.raise
          "Called [Fs_memo.init] a second time after a file watcher was \
           already declared as missing"
          [];
      Memo.Invalidation.empty
    | Waiting_for_file_watcher watch_records -> (
      match dune_file_watcher with
      | None ->
        state := No_file_watcher;
        Memo.Invalidation.empty
      | Some watcher ->
        state := File_watcher watcher;
        Memo.Invalidation.map_reduce watch_records
          ~f:(fun { accessed_path; path_to_watch } ->
            let path_to_watch = Path.outside_build_dir path_to_watch in
            watch_path watcher path_to_watch;
            invalidate accessed_path))
end

(* CR-someday amokhov: The current implementation doesn't handle symbolic links
   correctly. Instead of running [path_stat] on [path] directly and watching the
   [path] via its parent, we should watch all intermediate symbolic links, if
   there are any. If any of them changes, the whole chain should be invalidated
   and re-traversed/re-watched again. *)
let path_stat path =
  let* () = Watcher.watch ~try_to_watch_via_parent:true path in
  match Fs_cache.read Fs_cache.Untracked.path_stat path with
  | Ok { st_dev = _; st_ino = _; st_kind } as result when st_kind = S_DIR ->
    (* If [path] is a directory, we conservatively watch it directly too,
       because its stats may change in a way that doesn't trigger an event in
       the parent. We probably don't care about such changes for now because
       they correspond to the fields that are not included into [Reduced_stats].
       Still, to safeguard against future changes to [Reduced_stats], we watch
       the [path] directly. This doesn't cost us much since we would likely end
       up watching this directory anyway. *)
    let+ () = Watcher.watch ~try_to_watch_via_parent:false path in
    result
  | result -> Memo.return result

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
let path_kind path =
  path_stat path >>| function
  (* If the set of ignored fields below changes, it may be necessary to
     introduce a separate [Fs_cache.path_kind] primitive to avoid unnecessary
     restarts. *)
  | Ok { st_dev = _; st_ino = _; st_kind } -> Ok st_kind
  | Error _ as error -> error

let file_exists path =
  path_kind path >>| function
  | Ok kind -> File_kind.equal kind S_REG
  | Error (_ : Unix_error.Detailed.t) -> false

let is_directory path =
  path_kind path >>| function
  | Ok kind -> Ok (File_kind.equal kind S_DIR)
  | Error e -> Error e

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
    Cached_digest.Untracked.invalidate_cached_timestamp
      (Path.outside_build_dir path);
    Fs_cache.evict Fs_cache.Untracked.file_digest path);
  let+ () = Watcher.watch ~try_to_watch_via_parent:true path in
  Fs_cache.read Fs_cache.Untracked.file_digest path

let dir_contents ?(force_update = false) path =
  if force_update then Fs_cache.evict Fs_cache.Untracked.dir_contents path;
  let+ () = Watcher.watch ~try_to_watch_via_parent:false path in
  Fs_cache.read Fs_cache.Untracked.dir_contents path

(* CR-someday amokhov: For now, we do not cache the result of this operation
   because the result's type depends on [f]. There are only two call sites of
   [with_lexbuf_from_file], so perhaps we could just replace this more general
   function with two simpler ones that can be cached independently. *)
let tracking_file_digest path =
  let+ () = Watcher.watch ~try_to_watch_via_parent:true path in
  (* This is a bit of a hack. By reading [file_digest], we cause the [path] to
     be recorded in the [Fs_cache.Untracked.file_digest], so the build will be
     restarted if the digest changes. *)
  let (_ : Cached_digest.Digest_result.t) =
    Fs_cache.read Fs_cache.Untracked.file_digest path
  in
  ()

let with_lexbuf_from_file path ~f =
  let+ () = tracking_file_digest path in
  Io.Untracked.with_lexbuf_from_file (Path.outside_build_dir path) ~f

let file_contents path =
  let+ () = tracking_file_digest path in
  Io.read_file (Path.outside_build_dir path)

(* When a file or directory is created or deleted, we need to also invalidate
   the parent directory, so that the [dir_contents] queries are re-executed. *)
let invalidate_path_and_its_parent path =
  Memo.Invalidation.combine (Watcher.invalidate path)
    (match Path.Outside_build_dir.parent path with
    | None -> Memo.Invalidation.empty
    | Some path -> Watcher.invalidate path)

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
  let path = Path.as_outside_build_dir_exn path in
  match kind with
  | File_changed -> Watcher.invalidate path
  | Created | Deleted | Unknown -> invalidate_path_and_its_parent path

let init = Watcher.init
