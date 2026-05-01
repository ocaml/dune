open Import
open Memo.O
module Digest_result = Dune_digest.Digest_result

module Cached_digest = struct
  module Reduced_stats = struct
    type t =
      { mtime : Time.t
      ; size : int
      ; perm : Unix.file_perm
      ; dev : int
      ; ino : int
      }

    let repr =
      Repr.record
        "fs-memo-cached-digest-reduced-stats"
        [ Repr.field "mtime" Time.repr ~get:(fun t -> t.mtime)
        ; Repr.field "size" Repr.int ~get:(fun t -> t.size)
        ; Repr.field "perm" Repr.int ~get:(fun t -> t.perm)
        ; Repr.field "dev" Repr.int ~get:(fun t -> t.dev)
        ; Repr.field "ino" Repr.int ~get:(fun t -> t.ino)
        ]
    ;;

    let to_dyn = Repr.to_dyn repr

    let of_stat (stat : Stat.t) =
      { mtime = stat.mtime
      ; size = stat.size
      ; perm = stat.perm
      ; dev = stat.dev
      ; ino = stat.ino
      }
    ;;

    let _, compare = Repr.make_compare repr
  end

  type file =
    { mutable digest : Digest.t
    ; mutable stats : Reduced_stats.t
    ; mutable stats_checked : int
    }

  type t =
    { mutable checked_key : int
    ; mutable max_timestamp : Time.t
    ; table : file Path.Table.t
    }

  let db_file = Path.relative Path.build_dir ".digest-db"

  let file_repr =
    Repr.record
      "fs-memo-cached-digest-file"
      [ Repr.field "digest" (Repr.abstract Digest.to_dyn) ~get:(fun t -> t.digest)
      ; Repr.field "stats" Reduced_stats.repr ~get:(fun t -> t.stats)
      ; Repr.field "stats_checked" Repr.int ~get:(fun t -> t.stats_checked)
      ]
  ;;

  let file_to_dyn = Repr.to_dyn file_repr

  let repr =
    Repr.record
      "fs-memo-cached-digest"
      [ Repr.field "checked_key" Repr.int ~get:(fun t -> t.checked_key)
      ; Repr.field "max_timestamp" Time.repr ~get:(fun t -> t.max_timestamp)
      ; Repr.field
          "table"
          (Repr.abstract (Path.Table.to_dyn file_to_dyn))
          ~get:(fun t -> t.table)
      ]
  ;;

  let to_dyn = Repr.to_dyn repr

  module P = Persistent.Make (struct
      type nonrec t = t

      let name = "DIGEST-DB"
      let version = 10
      let sharing = true
      let to_dyn = to_dyn
    end)

  let needs_dumping = ref false

  (* CR-someday amokhov: replace this mutable table with a memoized function. This
     will probably require splitting this module in two, for dealing with source
     and target files, respectively. For source files, we receive updates via the
     file-watching API. For target files, we modify the digests ourselves, without
     subscribing for file-watching updates. *)
  let cache =
    lazy
      (match P.load db_file with
       | None ->
         { checked_key = 0; table = Path.Table.create (); max_timestamp = Time.of_ns 0 }
       | Some cache ->
         cache.checked_key <- cache.checked_key + 1;
         cache)
  ;;

  let get_current_filesystem_time () =
    let special_path = Path.relative Path.build_dir ".filesystem-clock" in
    Io.write_file special_path "<dummy>";
    (Stat.stat (Path.to_string special_path)).mtime
  ;;

  let wait_for_fs_clock_to_advance () =
    let t = get_current_filesystem_time () in
    while get_current_filesystem_time () <= t do
      (* This is a blocking wait but we don't care too much. This code is only
       used in the test suite. *)
      Unix.sleepf 0.01
    done
  ;;

  let delete_very_recent_entries () =
    let cache = Lazy.force cache in
    if !Clflags.wait_for_filesystem_clock then wait_for_fs_clock_to_advance ();
    let now = get_current_filesystem_time () in
    (* We can only trust digests with timestamps in the past. We had issues in
       the past with file systems having a slow internal clock, where we cached
       digests too aggressively. *)
    match Time.compare cache.max_timestamp now with
    | Lt -> ()
    | Eq | Gt ->
      let filter (data : file) =
        match Time.compare data.stats.mtime now with
        | Lt -> true
        | Gt | Eq -> false
      in
      (match Dune_trace.enabled Digest with
       | false -> Path.Table.filter_inplace cache.table ~f:filter
       | true ->
         let dropped = ref [] in
         Path.Table.filteri_inplace cache.table ~f:(fun ~key:path ~data ->
           let filter = filter data in
           if not filter then dropped := path :: !dropped;
           filter);
         (match !dropped with
          | [] -> ()
          | _ :: _ ->
            Dune_trace.emit ~buffered:true Digest (fun () ->
              Dune_trace.Event.Digest.dropped_stale_mtimes !dropped ~fs_now:now)))
  ;;

  let dump () =
    if !needs_dumping && Path.build_dir_exists ()
    then (
      needs_dumping := false;
      Console.Status_line.with_overlay
        (Live (fun () -> Pp.hbox (Pp.text "Saving digest db...")))
        ~f:(fun () ->
          delete_very_recent_entries ();
          P.dump db_file (Lazy.force cache)))
  ;;

  let () = At_exit.at_exit_ignore Dune_trace.at_exit dump

  let invalidate_cached_timestamps () =
    if Lazy.is_val cache
    then (
      let cache = Lazy.force cache in
      cache.checked_key <- cache.checked_key + 1);
    delete_very_recent_entries ()
  ;;

  let set_max_timestamp cache (stat : Stat.t) =
    cache.max_timestamp <- Time.max cache.max_timestamp stat.mtime
  ;;

  let set_with_stat path digest stat =
    let cache = Lazy.force cache in
    needs_dumping := true;
    set_max_timestamp cache stat;
    Path.Table.set
      cache.table
      path
      { digest; stats = Reduced_stats.of_stat stat; stats_checked = cache.checked_key }
  ;;

  let digest_path_with_stats path stats =
    match
      Digest.path_with_stats
        ~allow_dirs:true
        path
        (Digest.Stats_for_digest.of_time_stat stats)
    with
    | Ok digest -> Ok digest
    | Error Unexpected_kind -> Error (Digest_result.Error.Unexpected_kind stats.kind)
    | Error (Unix_error (ENOENT, _, _)) -> Error No_such_file
    | Error (Unix_error other_error) -> Error (Unix_error other_error)
  ;;

  (* Here we make only one [stat] call on the happy path. *)
  let refresh =
    let refresh_sync stats path =
      (* Note that by the time we reach this point, [stats] may become stale due to
         concurrent processes modifying the [path], so this function can actually
         return [No_such_file] even if the caller managed to obtain the [stats]. *)
      let result = digest_path_with_stats path stats in
      Result.iter result ~f:(fun digest -> set_with_stat path digest stats);
      result
    in
    fun path ->
      let path_string = Path.to_string path in
      Digest_result.catch_fs_errors (fun () ->
        match Stat.stat path_string with
        | stats -> refresh_sync stats path
        | exception Unix.Unix_error (ENOENT, _, _) ->
          (* Test if this is a broken symlink for better error messages. *)
          (match Unix.lstat path_string with
           | exception Unix.Unix_error (ENOENT, _, _) -> Error No_such_file
           | _stats_so_must_be_a_symlink -> Error Broken_symlink))
  ;;

  let peek_file path =
    let cache = Lazy.force cache in
    match Path.Table.find cache.table path with
    | None -> None
    | Some x ->
      Some
        (if x.stats_checked = cache.checked_key
         then Ok x.digest
         else (
           (* The [stat] below follows symlinks. *)
           let path_string = Path.to_string path in
           match
             Dune_digest.Digest_result.catch_fs_errors (fun () ->
               match Stat.stat path_string with
               | exception Unix.Unix_error (ENOENT, _, _) -> Error No_such_file
               | stats -> Ok stats)
           with
           | Error e -> Error e
           | Ok stats ->
             let reduced_stats = Reduced_stats.of_stat stats in
             (match Reduced_stats.compare x.stats reduced_stats with
              | Eq ->
                (* Even though we're modifying the [stats_checked] field, we don't
                   need to set [needs_dumping := true] here. This is because
                   [checked_key] is incremented every time we load from disk, which
                   makes it so that [stats_checked < checked_key] for all entries
                   after loading, regardless of whether we save the new value here
                   or not. *)
                x.stats_checked <- cache.checked_key;
                Ok x.digest
              | Gt | Lt ->
                let digest_result = digest_path_with_stats path stats in
                Result.iter digest_result ~f:(fun digest ->
                  Dune_trace.emit ~buffered:true Digest (fun () ->
                    Dune_trace.Event.Digest.redigest
                      ~path
                      ~old_digest:(Digest.to_string x.digest)
                      ~new_digest:(Digest.to_string digest)
                      ~old_stats:(Reduced_stats.to_dyn x.stats)
                      ~new_stats:(Reduced_stats.to_dyn reduced_stats));
                  needs_dumping := true;
                  set_max_timestamp cache stats;
                  x.digest <- digest;
                  x.stats <- reduced_stats;
                  x.stats_checked <- cache.checked_key);
                digest_result)))
  ;;

  module Untracked = struct
    let source_or_external_file path =
      let path = Path.outside_build_dir path in
      match peek_file path with
      | Some digest_result -> digest_result
      | None -> refresh path
    ;;

    let invalidate_cached_timestamp path =
      let path = Path.outside_build_dir path in
      let cache = Lazy.force cache in
      match Path.Table.find cache.table path with
      | None -> ()
      | Some entry ->
        (* Make [stats_checked] unequal to [cache.checked_key] so that [peek_file]
         is forced to re-[stat] the [path]. *)
        let entry = { entry with stats_checked = cache.checked_key - 1 } in
        Path.Table.set cache.table path entry
    ;;
  end

  let load () = P.load db_file

  let entries { table; _ } =
    let entries = ref [] in
    Path.Table.filteri_inplace table ~f:(fun ~key ~data ->
      entries := (key, data) :: !entries;
      true);
    List.sort !entries ~compare:(fun (path_a, _) (path_b, _) ->
      Path.compare path_a path_b)
  ;;
end

let invalidate_cached_timestamps = Cached_digest.invalidate_cached_timestamps

module Debug = struct
  type selector =
    | Exact of Path.t
    | Direct_children_of of Path.t

  let selectors paths =
    List.map paths ~f:(fun path ->
      match
        Unix_error.Detailed.catch (fun path -> Unix.stat (Path.to_string path)) path
      with
      | Ok { Unix.st_kind = S_DIR; _ } -> Direct_children_of path
      | Ok { Unix.st_kind = _; _ } | Error _ -> Exact path)
  ;;

  let matches selectors path =
    match selectors with
    | [] -> true
    | selectors ->
      List.exists selectors ~f:(function
        | Exact selected -> Path.equal path selected
        | Direct_children_of dir ->
          (match Path.parent path with
           | Some parent -> Path.equal parent dir
           | None -> false))
  ;;

  let selected_entries db paths =
    let selectors = selectors paths in
    Cached_digest.entries db |> List.filter ~f:(fun (path, _) -> matches selectors path)
  ;;

  let entry_repr =
    Repr.record
      "fs-memo-debug-entry"
      [ Repr.field "path" (Repr.abstract Path.to_dyn) ~get:(fun (path, _) -> path)
      ; Repr.field
          "digest"
          (Repr.abstract Digest.to_dyn)
          ~get:(fun (_, { Cached_digest.digest; _ }) -> digest)
      ; Repr.field
          "stats"
          Cached_digest.Reduced_stats.repr
          ~get:(fun (_, { Cached_digest.stats; _ }) -> stats)
      ; Repr.field
          "stats_checked"
          Repr.int
          ~get:(fun (_, { Cached_digest.stats_checked; _ }) -> stats_checked)
      ]
  ;;

  let entry_to_dyn = Repr.to_dyn entry_repr

  let load_exn () =
    match Cached_digest.load () with
    | Some db -> db
    | None ->
      User_error.raise
        [ Pp.textf
            "No digest database found at %s"
            (Path.to_string_maybe_quoted Cached_digest.db_file)
        ]
  ;;

  let dump_digest_db paths =
    let db = load_exn () in
    let { Cached_digest.checked_key; max_timestamp; _ } = db in
    let entries =
      selected_entries db paths
      |> List.map ~f:(fun (path, file) -> entry_to_dyn (path, file))
    in
    Dyn.Record
      [ "checked_key", Int checked_key
      ; "max_timestamp", Int (Time.to_ns max_timestamp)
      ; "entries", List entries
      ]
  ;;

  type finding =
    { status : string
    ; path : Path.t
    ; cached_digest : Digest.t
    ; actual : Digest_result.t
    }

  let current_digest path =
    let path_string = Path.to_string path in
    match
      Digest_result.catch_fs_errors (fun () ->
        match Stat.stat path_string with
        | stats ->
          Ok
            ( Some (Cached_digest.Reduced_stats.of_stat stats)
            , Cached_digest.digest_path_with_stats path stats )
        | exception Unix.Unix_error (ENOENT, _, _) ->
          (match Unix.lstat path_string with
           | exception Unix.Unix_error (ENOENT, _, _) ->
             Error Digest_result.Error.No_such_file
           | _ -> Error Digest_result.Error.Broken_symlink))
    with
    | Ok current -> current
    | Error error -> None, Error error
  ;;

  let finding_repr =
    Repr.record
      "fs-memo-debug-finding"
      [ Repr.field "status" Repr.string ~get:(fun t -> t.status)
      ; Repr.field "path" (Repr.abstract Path.to_dyn) ~get:(fun t -> t.path)
      ; Repr.field "cached_digest" (Repr.abstract Digest.to_dyn) ~get:(fun t ->
          t.cached_digest)
      ; Repr.field "actual" (Repr.abstract Digest_result.to_dyn) ~get:(fun t -> t.actual)
      ]
  ;;

  let finding_to_dyn = Repr.to_dyn finding_repr

  let check_digest_db paths =
    let db = load_exn () in
    selected_entries db paths
    |> List.filter_map ~f:(fun (path, file) ->
      let { Cached_digest.digest = cached_digest; stats; stats_checked = _ } = file in
      let current_stats, actual = current_digest path in
      if Digest_result.equal actual (Ok cached_digest)
      then None
      else (
        let status =
          match current_stats with
          | Some current_stats
            when Cached_digest.Reduced_stats.compare stats current_stats = Eq -> "invalid"
          | Some _ | None -> "stale"
        in
        Some (finding_to_dyn { status; path; cached_digest; actual })))
    |> fun findings -> Dyn.List findings
  ;;
end

module Dir_contents = struct
  (* CR-someday amokhov: Using a [Filename.Map] instead of a list would be better
     since we'll not need to worry about the invariant that the list is sorted
     and doesn't contain any duplicate file names. Using maps will likely be
     more costly, so we need to do some benchmarking before switching. *)
  type t = (Filename.t * File_kind.t) list

  let to_list t = t
  let iter t = List.iter t

  (* The names must be unique, so we don't care about comparing file kinds. *)
  let of_list = List.sort ~compare:(fun (x, _) (y, _) -> Filename.compare x y)
  let equal = List.equal (Tuple.T2.equal Filename.equal File_kind.equal)
end

(* This module caches only a subset of fields of [Unix.stats] because other
   fields are currently unused.

   Note that we specifically do not want to cache [mtime] and [ctime] because
   these fields can change too often: for example, when a temporary file is
   created in a watched directory. *)
module Reduced_stats = struct
  type t =
    { st_dev : int
    ; st_ino : int
    ; st_kind : Unix.file_kind
    }

  let of_unix_stats { Unix.st_dev; st_ino; st_kind; _ } = { st_dev; st_ino; st_kind }

  let equal x y =
    Int.equal x.st_dev y.st_dev
    && Int.equal x.st_ino y.st_ino
    && File_kind.equal x.st_kind y.st_kind
  ;;
end

module Fs_cache = struct
  (* CR-someday amokhov: Persistently store the caches of (some?) operations. *)

  (* CR-someday amokhov: Implement garbage collection. *)

  (** A cached file-system operation on a [Path.Outside_build_dir.t] whose result
    type is ['a]. For example, an operation to check if a path exists returns
    ['a = bool].

    Currently we do not expose a way to construct such cached operations; see
    the [Untracked] module for a few predefined ones. *)

  type 'a t =
    { name : string (* For debugging *)
    ; sample : Path.Outside_build_dir.t -> 'a
    ; cache : 'a Path.Outside_build_dir.Table.t
    ; equal : 'a -> 'a -> bool (* Used to implement cutoff *)
    }

  let create name ~sample ~equal : 'a t =
    { name; sample; equal; cache = Path.Outside_build_dir.Table.create 128 }
  ;;

  (* If the cache contains the result of applying an operation to a path, return
     it. Otherwise, perform the operation, store the result in the cache, and
     then return it. *)
  let read { sample; cache; _ } path =
    match Path.Outside_build_dir.Table.find cache path with
    | Some cached_result -> cached_result
    | None ->
      let result = sample path in
      Path.Outside_build_dir.Table.add_exn cache path result;
      result
  ;;

  let evict { cache; _ } path = Path.Outside_build_dir.Table.remove cache path

  let update { sample; cache; equal; name } path =
    let result =
      match Path.Outside_build_dir.Table.find cache path with
      | None -> `Skipped
      | Some old_result ->
        let new_result = sample path in
        if equal old_result new_result
        then `Unchanged
        else (
          Path.Outside_build_dir.Table.set cache path new_result;
          `Changed)
    in
    Dune_trace.emit ~buffered:true Cache (fun () ->
      Dune_trace.Event.Cache.fs_update ~cache_type:name ~path result);
    result
  ;;

  module Untracked = struct
    (* A few predefined cached operations. They are "untracked" in the sense that
       the user is responsible for tracking the file system and manually calling
       the [update] function to bring the stored results up to date.

       See later in this module for tracked versions of these operations. *)
    let path_stat =
      let sample path =
        Path.outside_build_dir path
        |> Path.Untracked.stat
        |> Result.map ~f:Reduced_stats.of_unix_stats
      in
      create
        "path_stat"
        ~sample
        ~equal:(Result.equal Reduced_stats.equal Unix_error.Detailed.equal)
    ;;

    (* CR-someday amokhov: There is an overlap in functionality between this
       module and [cached_digest.ml]. In particular, digests are stored twice,
      in two separate tables. We should find a way to merge the tables into one.
    *)
    let file_digest =
      let sample p = Cached_digest.Untracked.source_or_external_file p in
      create "file_digest" ~sample ~equal:Digest_result.equal
    ;;

    let dir_contents =
      create
        "dir_contents"
        ~sample:(fun path ->
          Path.Untracked.readdir_unsorted_with_kinds (Path.outside_build_dir path)
          |> Result.map ~f:Dir_contents.of_list)
        ~equal:(Result.equal Dir_contents.equal Unix_error.Detailed.equal)
    ;;
  end
end

(* Watching and invalidating paths. *)
module Watcher : sig
  val init : dune_file_watcher:Dune_scheduler.File_watcher.t option -> Memo.Invalidation.t

  (* Watch a path. You should call this function *before* accessing the file
     system to prevent possible races.

     If [try_to_watch_via_parent = true], the watcher will attempt to watch the
     path by watching its parent directory, falling back to watching the path
     directly if there is no parent. This is an optimisation that allows us to
     reduce the number of watched paths: typically, the number of directories is
     a lot smaller than the number of files. *)
  val watch : try_to_watch_via_parent:bool -> Path.Outside_build_dir.t -> unit Memo.t

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

     - [File_watcher] holds [Dune_scheduler.File_watcher.t] once it has been initialised
       and all previously collected [watched_record]s have been passed to it. *)
  type state =
    | Waiting_for_file_watcher of watch_record list
    | No_file_watcher
    | File_watcher of Dune_scheduler.File_watcher.t

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
    match Dune_scheduler.File_watcher.add_watch watcher path with
    | Ok () -> ()
    | Error `Does_not_exist ->
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
      (match Dune_scheduler.File_watcher.add_watch watcher containing_dir with
       | Ok () -> ()
       | Error `Does_not_exist ->
         Log.info
           "Attempted to add watch to non-existent directory"
           [ "path", Dyn.string (Path.to_string containing_dir) ]);
      (match Dune_scheduler.File_watcher.add_watch watcher path with
       | Error `Does_not_exist | Ok () -> ())
  ;;

  let watch_or_record_path ~accessed_path ~path_to_watch =
    match !state with
    | Waiting_for_file_watcher watch_records ->
      state := Waiting_for_file_watcher ({ accessed_path; path_to_watch } :: watch_records)
    | No_file_watcher -> ()
    | File_watcher dune_file_watcher ->
      let path_to_watch = Path.outside_build_dir path_to_watch in
      watch_path dune_file_watcher path_to_watch
  ;;

  (* This comment applies to both memoization tables below.

     It may seem weird that we are adding a watch on every invalidation of the
     cell. This is OK because [Dune_scheduler.File_watcher.add_watch] is idempotent, in
     the sense that we are not accumulating watches. In fact, if a path
     disappears then we lose the watch and have to re-establish it, so doing it
     on every computation is sometimes necessary. *)
  let memo_for_watching_directly =
    Memo.create
      "fs_memo_for_watching_directly"
      ~input:(module Path.Outside_build_dir)
      (fun accessed_path ->
         watch_or_record_path ~accessed_path ~path_to_watch:accessed_path;
         Memo.return ())
  ;;

  let memo_for_watching_via_parent =
    Memo.create
      "fs_memo_for_watching_via_parent"
      ~input:(module Path.Outside_build_dir)
      (fun accessed_path ->
         let path_to_watch =
           Option.value
             (Path.Outside_build_dir.parent accessed_path)
             ~default:accessed_path
         in
         watch_or_record_path ~accessed_path ~path_to_watch;
         Memo.return ())
  ;;

  let watch ~try_to_watch_via_parent path =
    match try_to_watch_via_parent with
    | false -> Memo.exec memo_for_watching_directly path
    | true -> Memo.exec memo_for_watching_via_parent path
  ;;

  let update_all p =
    Cached_digest.Untracked.invalidate_cached_timestamp p;
    let dir_contents = Fs_cache.(update Untracked.dir_contents) p in
    let digest = Fs_cache.(update Untracked.file_digest) p in
    let stat = Fs_cache.(update Untracked.path_stat) p in
    List.fold_left [ stat; digest; dir_contents ] ~init:`Skipped ~f:(fun x y ->
      match x, y with
      | `Skipped, res | res, `Skipped -> res
      | `Changed, _ | _, `Changed -> `Changed
      | `Unchanged, `Unchanged -> `Unchanged)
  ;;

  (* CR-someday amokhov: We share Memo tables for tracking different file-system
     operations. This saves some memory, but leads to recomputing more memoized
     functions than necessary. We can use a separate table for each [Fs_cache]
     operation, or even better use [Fs_cache] tables in Memo directory, e.g. via
     [Memo.create_with_store]. *)
  let invalidate path =
    match update_all path with
    | `Skipped | `Unchanged -> Memo.Invalidation.empty
    | `Changed ->
      let reason : Memo.Invalidation.Reason.t =
        Path_changed (Path.outside_build_dir path)
      in
      Memo.Invalidation.combine
        (Memo.Cell.invalidate (Memo.cell memo_for_watching_directly path) ~reason)
        (Memo.Cell.invalidate (Memo.cell memo_for_watching_via_parent path) ~reason)
  ;;

  let init ~dune_file_watcher =
    match !state with
    | File_watcher _ ->
      Code_error.raise
        "Called [Fs_memo.init] a second time after a file watcher was already set up"
        []
    | No_file_watcher ->
      (* It would be nice to disallow this branch to simplify things, but there
         are tests that call [Scheduler.go] multiple times, therefore [init]
         gets called multiple times with [dune_file_watcher = None]. Since they
         don't use the file watcher, it shouldn't be a problem. *)
      if Option.is_some dune_file_watcher
      then
        Code_error.raise
          "Called [Fs_memo.init] a second time after a file watcher was already declared \
           as missing"
          [];
      Memo.Invalidation.empty
    | Waiting_for_file_watcher watch_records ->
      (match dune_file_watcher with
       | None ->
         state := No_file_watcher;
         Memo.Invalidation.empty
       | Some watcher ->
         state := File_watcher watcher;
         Memo.Invalidation.map_reduce
           watch_records
           ~f:(fun { accessed_path; path_to_watch } ->
             let path_to_watch = Path.outside_build_dir path_to_watch in
             watch_path watcher path_to_watch;
             invalidate accessed_path))
  ;;
end

(* CR-someday amokhov: The current implementation doesn't handle symbolic links
   correctly. Instead of running [path_stat] on [path] directly and watching the
   [path] via its parent, we should watch all intermediate symbolic links, if
   there are any. If any of them changes, the whole chain should be invalidated
   and re-traversed/re-watched again. *)
let path_stat path =
  let* () = Watcher.watch ~try_to_watch_via_parent:true path in
  match Fs_cache.read Fs_cache.Untracked.path_stat path with
  | Ok { st_dev = _; st_ino = _; st_kind = S_DIR } as result ->
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
;;

(* We currently implement [file_exists] and [dir_exists] functions by calling
   [Fs_cache.path_stat] instead of creating separate [Fs_cache] primitives. Here
   are some reasons for doing this:

   - Semantically, this is equivalent because [Fpath.exists] is also implemented
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
  path_stat path
  >>| function
  (* If the set of ignored fields below changes, it may be necessary to
     introduce a separate [Fs_cache.path_kind] primitive to avoid unnecessary
     restarts. *)
  | Ok { st_dev = _; st_ino = _; st_kind } -> Ok st_kind
  | Error _ as error -> error
;;

let file_exists path =
  path_kind path
  >>| function
  | Ok kind -> File_kind.equal kind S_REG
  | Error (_ : Unix_error.Detailed.t) -> false
;;

let is_directory path =
  path_kind path
  >>| function
  | Ok kind -> Ok (File_kind.equal kind S_DIR)
  | Error e -> Error e
;;

let dir_exists path =
  path_kind path
  >>| function
  | Ok kind -> File_kind.equal kind S_DIR
  | Error (_ : Unix_error.Detailed.t) -> false
;;

(* CR-someday amokhov: It is unclear if we got the layers of abstraction right
   here. One could argue that caching is a higher-level concept compared to file
   watching, and we should expose this function from the [Cached_digest] module
   instead. For now, we keep it here because it seems nice to group all tracked
   file system access functions in one place, and exposing an uncached version
   of [file_digest] seems error-prone. We may need to rethink this decision. *)
let file_digest ?(force_update = false) path =
  if force_update
  then (
    Cached_digest.Untracked.invalidate_cached_timestamp path;
    Fs_cache.evict Fs_cache.Untracked.file_digest path);
  let+ () = Watcher.watch ~try_to_watch_via_parent:true path in
  Fs_cache.read Fs_cache.Untracked.file_digest path
;;

let file_digest_exn ~loc path =
  let report_user_error details =
    let+ loc = loc () in
    User_error.raise
      ?loc
      ([ Pp.textf
           "File unavailable: %s"
           (Path.Outside_build_dir.to_string_maybe_quoted path)
       ]
       @ details)
  in
  file_digest path
  >>= function
  | Ok digest -> Memo.return digest
  | Error e ->
    if Digest_result.Error.no_such_file e
    then report_user_error []
    else report_user_error [ Digest_result.Error.pp e (Path.outside_build_dir path) ]
;;

let dir_contents ?(force_update = false) path =
  if force_update then Fs_cache.evict Fs_cache.Untracked.dir_contents path;
  let+ () = Watcher.watch ~try_to_watch_via_parent:false path in
  Fs_cache.read Fs_cache.Untracked.dir_contents path
;;

(* CR-someday amokhov: For now, we do not cache the result of this operation
   because the result's type depends on [f]. There are only two call sites of
   [with_lexbuf_from_file], so perhaps we could just replace this more general
   function with two simpler ones that can be cached independently. *)
let tracking_file_digest path =
  let+ () = Watcher.watch ~try_to_watch_via_parent:true path in
  (* This is a bit of a hack. By reading [file_digest], we cause the [path] to
     be recorded in the [Fs_cache.Untracked.file_digest], so the build will be
     restarted if the digest changes. *)
  let (_ : Digest_result.t) = Fs_cache.read Fs_cache.Untracked.file_digest path in
  ()
;;

let with_lexbuf_from_file path ~f =
  let+ () = tracking_file_digest path in
  Io.Untracked.with_lexbuf_from_file (Path.outside_build_dir path) ~f
;;

let file_contents path =
  let+ () = tracking_file_digest path in
  Io.read_file (Path.outside_build_dir path)
;;

(* When a file or directory is created or deleted, we need to also invalidate
   the parent directory, so that the [dir_contents] queries are re-executed. *)
let invalidate_path_and_its_parent path =
  Memo.Invalidation.combine
    (Watcher.invalidate path)
    (match Path.Outside_build_dir.parent path with
     | None -> Memo.Invalidation.empty
     | Some path -> Watcher.invalidate path)
;;

(* CR-someday amokhov: The way we currently treat file system events is simple
   and robust but doesn't take advantage of all the information we receive. Here
   are some ideas for future optimisation:

   - Don't invalidate [file_exists] queries on [File_changed] events.

   - If [file_exists] currently returns [true] and we receive a corresponding
     [Deleted] event, we can change the result to [false] without rerunning the
     [Fpath.exists] function. Similarly for the case where [file_exists] is
     [false] and we receive a corresponding [Created] event.

   - Finally, the result of [dir_contents] queries can be updated without
     calling [Path.Untracked.readdir_unsorted_with_kinds]: we know which file or
     directory should be added to or removed from the result. *)
let handle_fs_event ({ kind; path } : Dune_scheduler.File_watcher.Fs_memo_event.t)
  : Memo.Invalidation.t
  =
  match Path.destruct_build_dir path with
  | `Inside _ ->
    (* This can occur on MacOS when [PATH=.:$PATH] for example *)
    Platform.assert_os Darwin;
    Memo.Invalidation.empty
  | `Outside path ->
    (match kind with
     | File_changed -> Watcher.invalidate path
     | Created | Deleted | Unknown -> invalidate_path_and_its_parent path)
;;

let init = Watcher.init

(* Register the Fs_memo implementation with the scheduler *)
let () = Dune_scheduler.Scheduler.set_fs_memo_impl ~handle_fs_event ~init

module Untracked = struct
  let file_digest = Fs_cache.read Fs_cache.Untracked.file_digest
  let path_stat = Fs_cache.read Fs_cache.Untracked.path_stat
end
