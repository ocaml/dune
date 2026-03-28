open Import

type file =
  { mutable digest : Digest.t
  ; mutable stats : Reduced_stats.t
  ; mutable stats_checked : int
  }

type t =
  { mutable checked_key : int
  ; mutable max_timestamp : float
  ; table : file Path.Table.t
  }

let db_file = Path.relative Path.build_dir ".digest-db"

let dyn_of_file { digest; stats; stats_checked } =
  Dyn.Record
    [ "digest", Digest.to_dyn digest
    ; "stats", Reduced_stats.to_dyn stats
    ; "stats_checked", Int stats_checked
    ]
;;

let to_dyn { checked_key; max_timestamp; table } =
  Dyn.Record
    [ "checked_key", Int checked_key
    ; "max_timestamp", Float max_timestamp
    ; "table", Path.Table.to_dyn dyn_of_file table
    ]
;;

module P = Persistent.Make (struct
    type nonrec t = t

    let name = "DIGEST-DB"
    let version = 8
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
     | None -> { checked_key = 0; table = Path.Table.create (); max_timestamp = 0. }
     | Some cache ->
       cache.checked_key <- cache.checked_key + 1;
       cache)
;;

let get_current_filesystem_time () =
  let special_path = Path.relative Path.build_dir ".filesystem-clock" in
  Io.write_file special_path "<dummy>";
  (Unix.stat (Path.to_string special_path)).st_mtime
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
  match Float.compare cache.max_timestamp now with
  | Lt -> ()
  | Eq | Gt ->
    let filter (data : file) =
      match Float.compare data.stats.mtime now with
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

let set_max_timestamp cache (stat : Unix.stats) =
  cache.max_timestamp <- Float.max cache.max_timestamp stat.st_mtime
;;

let set_with_stat path digest stat =
  let cache = Lazy.force cache in
  needs_dumping := true;
  set_max_timestamp cache stat;
  Path.Table.set
    cache.table
    path
    { digest
    ; stats = Reduced_stats.of_unix_stats stat
    ; stats_checked = cache.checked_key
    }
;;

let digest_path_with_stats ~allow_dirs path stats =
  match
    Digest.path_with_stats ~allow_dirs path (Digest.Stats_for_digest.of_unix_stats stats)
  with
  | Ok digest -> Ok digest
  | Error Unexpected_kind -> Error (Digest_result.Error.Unexpected_kind stats.st_kind)
  | Error (Unix_error (ENOENT, _, _)) -> Error No_such_file
  | Error (Unix_error other_error) -> Error (Unix_error other_error)
;;

(* Here we make only one [stat] call on the happy path. *)
let refresh_without_removing_write_permissions =
  let refresh_sync ~allow_dirs stats path =
    (* Note that by the time we reach this point, [stats] may become stale due to
     concurrent processes modifying the [path], so this function can actually
     return [No_such_file] even if the caller managed to obtain the [stats]. *)
    let result = digest_path_with_stats ~allow_dirs path stats in
    Result.iter result ~f:(fun digest -> set_with_stat path digest stats);
    result
  in
  fun ~allow_dirs path ->
    Digest_result.catch_fs_errors (fun () ->
      match Unix.stat (Path.to_string path) with
      | stats -> refresh_sync stats ~allow_dirs path
      | exception Unix.Unix_error (ELOOP, _, _) -> Error Cyclic_symlink
      | exception Unix.Unix_error (ENOENT, _, _) ->
        (* Test if this is a broken symlink for better error messages. *)
        (match Unix.lstat (Path.to_string path) with
         | exception Unix.Unix_error (ENOENT, _, _) -> Error No_such_file
         | _stats_so_must_be_a_symlink -> Error Broken_symlink))
;;

(* CR-someday amokhov: We do [lstat] followed by [stat] only because we do not
   want to remove write permissions from the symbolic link's target, which may
   be outside of the build directory and not under out control. It seems like it
   should be possible to avoid paying for two system calls ([lstat] and [stat])
   here, e.g., by telling the subsequent [chmod] to not follow symlinks. *)

let peek_file ~allow_dirs path =
  let cache = Lazy.force cache in
  match Path.Table.find cache.table path with
  | None -> None
  | Some x ->
    Some
      (if x.stats_checked = cache.checked_key
       then Ok x.digest
       else (
         (* The [stat] below follows symlinks. *)
         match Unix.stat (Path.to_string path) with
         | exception Unix.Unix_error (ELOOP, _, _) ->
           Error Digest_result.Error.Cyclic_symlink
         | exception Unix.Unix_error (ENOENT, _, _) -> Error No_such_file
         | exception Unix.Unix_error (error, syscall, arg) ->
           Error (Unix_error (Unix_error.Detailed.create ~syscall ~arg error))
         | exception exn -> Error (Unrecognized exn)
         | stats ->
           let reduced_stats = Reduced_stats.of_unix_stats stats in
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
              let digest_result = digest_path_with_stats ~allow_dirs path stats in
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
    match peek_file ~allow_dirs:true path with
    | Some digest_result -> digest_result
    | None -> refresh_without_removing_write_permissions ~allow_dirs:true path
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
