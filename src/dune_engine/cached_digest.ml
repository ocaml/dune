open Import

module Reduced_stats = struct
  type t =
    { mtime : float
    ; size : int
    ; perm : Unix.file_perm
    }

  let to_dyn { mtime; size; perm } =
    Dyn.Record [ "mtime", Float mtime; "size", Int size; "perm", Int perm ]
  ;;

  let of_unix_stats (stats : Unix.stats) =
    { mtime = stats.st_mtime; size = stats.st_size; perm = stats.st_perm }
  ;;

  let compare { mtime; size; perm } t =
    let open Ordering.O in
    let= () = Float.compare mtime t.mtime in
    let= () = Int.compare size t.size in
    Int.compare perm t.perm
  ;;
end

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
    let version = 6
    let to_dyn = to_dyn

    let test_example () =
      let table = Path.Table.create () in
      Path.Table.set
        table
        (Path.external_ (Path.External.of_string "/"))
        { stats_checked = 1
        ; digest = Digest.string "xxx"
        ; stats = { Reduced_stats.mtime = 0.; size = 1; perm = 0 }
        };
      { checked_key = 1; max_timestamp = 0.; table }
    ;;
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
  (Path.Untracked.stat_exn special_path).st_mtime
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
    (match !Clflags.debug_digests with
     | false -> Path.Table.filter_inplace cache.table ~f:filter
     | true ->
       Path.Table.filteri_inplace cache.table ~f:(fun ~key:path ~data ->
         let filter = filter data in
         if not filter
         then
           Console.print
             [ Pp.textf
                 "Dropping cached digest for %s because it has exactly the same mtime as \
                  the file system clock."
                 (Path.to_string_maybe_quoted path)
             ];
         filter))
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

let () = at_exit dump

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

let set path digest =
  (* the caller of [set] ensures that the files exist *)
  let path = Path.build path in
  let stat = Path.Untracked.stat_exn path in
  set_with_stat path digest stat
;;

module Digest_result = struct
  type t =
    | Ok of Digest.t
    | No_such_file
    | Broken_symlink
    | Cyclic_symlink
    | Unexpected_kind of File_kind.t
    | Unix_error of Unix_error.Detailed.t
    | Error of exn

  let equal x y =
    match x, y with
    | Ok x, Ok y -> Digest.equal x y
    | Ok _, _ | _, Ok _ -> false
    | No_such_file, No_such_file -> true
    | No_such_file, _ | _, No_such_file -> false
    | Broken_symlink, Broken_symlink -> true
    | Broken_symlink, _ | _, Broken_symlink -> false
    | Cyclic_symlink, Cyclic_symlink -> true
    | Cyclic_symlink, _ | _, Cyclic_symlink -> false
    | Unexpected_kind x, Unexpected_kind y -> File_kind.equal x y
    | Unexpected_kind _, _ | _, Unexpected_kind _ -> false
    | Unix_error x, Unix_error y ->
      Tuple.T3.equal Unix_error.equal String.equal String.equal x y
    | Unix_error _, _ | _, Unix_error _ -> false
    | Error x, Error y ->
      (* Falling back to polymorphic equality check seems OK for this rare case.
         We could also just return [false] but that would break the reflexivity
         of the equality check, which doesn't seem nice. *)
      x = y
  ;;

  let to_option = function
    | Ok t -> Some t
    | No_such_file
    | Broken_symlink
    | Cyclic_symlink
    | Unexpected_kind _
    | Unix_error _
    | Error _ -> None
  ;;

  let iter t ~f = Option.iter (to_option t) ~f

  let to_dyn = function
    | Ok digest -> Dyn.Variant ("Ok", [ Digest.to_dyn digest ])
    | No_such_file -> Variant ("No_such_file", [])
    | Broken_symlink -> Variant ("Broken_symlink", [])
    | Cyclic_symlink -> Variant ("Cyclic_symlink", [])
    | Unexpected_kind kind -> Variant ("Unexpected_kind", [ File_kind.to_dyn kind ])
    | Unix_error error -> Variant ("Unix_error", [ Unix_error.Detailed.to_dyn error ])
    | Error exn -> Variant ("Error", [ String (Printexc.to_string exn) ])
  ;;
end

let digest_path_with_stats ~allow_dirs path stats =
  match
    Digest.path_with_stats ~allow_dirs path (Digest.Stats_for_digest.of_unix_stats stats)
  with
  | Ok digest -> Digest_result.Ok digest
  | Unexpected_kind -> Unexpected_kind stats.st_kind
  | Unix_error (ENOENT, _, _) -> No_such_file
  | Unix_error other_error -> Unix_error other_error
;;

let refresh ~allow_dirs stats path =
  (* Note that by the time we reach this point, [stats] may become stale due to
     concurrent processes modifying the [path], so this function can actually
     return [No_such_file] even if the caller managed to obtain the [stats]. *)
  let result = digest_path_with_stats ~allow_dirs path stats in
  Digest_result.iter result ~f:(fun digest -> set_with_stat path digest stats);
  result
;;

let catch_fs_errors f =
  match f () with
  | result -> result
  | exception Unix.Unix_error (error, syscall, arg) ->
    Digest_result.Unix_error (error, syscall, arg)
  | exception exn -> Error exn
;;

(* Here we make only one [stat] call on the happy path. *)
let refresh_without_removing_write_permissions ~allow_dirs path =
  catch_fs_errors (fun () ->
    match Path.Untracked.stat_exn path with
    | stats -> refresh stats ~allow_dirs path
    | exception Unix.Unix_error (ELOOP, _, _) -> Cyclic_symlink
    | exception Unix.Unix_error (ENOENT, _, _) ->
      (* Test if this is a broken symlink for better error messages. *)
      (match Path.Untracked.lstat_exn path with
       | exception Unix.Unix_error (ENOENT, _, _) -> No_such_file
       | _stats_so_must_be_a_symlink -> Broken_symlink))
;;

(* CR-someday amokhov: We do [lstat] followed by [stat] only because we do not
   want to remove write permissions from the symbolic link's target, which may
   be outside of the build directory and not under out control. It seems like it
   should be possible to avoid paying for two system calls ([lstat] and [stat])
   here, e.g., by telling the subsequent [chmod] to not follow symlinks. *)
let refresh_and_remove_write_permissions ~allow_dirs path =
  catch_fs_errors (fun () ->
    match Path.Untracked.lstat_exn path with
    | exception Unix.Unix_error (ENOENT, _, _) -> No_such_file
    | stats ->
      (match stats.st_kind with
       | S_LNK ->
         (match Path.Untracked.stat_exn path with
          | stats -> refresh stats ~allow_dirs:false path
          | exception Unix.Unix_error (ELOOP, _, _) -> Cyclic_symlink
          | exception Unix.Unix_error (ENOENT, _, _) -> Broken_symlink)
       | (S_DIR | S_REG) as kind ->
         let perm = Path.Permissions.remove Path.Permissions.write stats.st_perm in
         Path.chmod ~mode:perm path;
         let allow_dirs = kind = S_DIR in
         refresh ~allow_dirs { stats with st_perm = perm } path
       | _ ->
         (* CR-someday amokhov: Shall we proceed if [stats.st_kind = S_DIR]?
            What about stranger kinds like [S_SOCK]? *)
         refresh ~allow_dirs stats path))
;;

let refresh ~allow_dirs ~remove_write_permissions path =
  let path = Path.build path in
  match remove_write_permissions with
  | false -> refresh_without_removing_write_permissions ~allow_dirs path
  | true -> refresh_and_remove_write_permissions ~allow_dirs path
;;

let peek_file ~allow_dirs path =
  let cache = Lazy.force cache in
  match Path.Table.find cache.table path with
  | None -> None
  | Some x ->
    Some
      (if x.stats_checked = cache.checked_key
       then Digest_result.Ok x.digest
       else (
         (* The [stat_exn] below follows symlinks. *)
         match Path.Untracked.stat_exn path with
         | exception Unix.Unix_error (ELOOP, _, _) -> Cyclic_symlink
         | exception Unix.Unix_error (ENOENT, _, _) -> No_such_file
         | exception Unix.Unix_error (error, syscall, arg) ->
           Unix_error (Unix_error.Detailed.create ~syscall ~arg error)
         | exception exn -> Error exn
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
              Digest_result.iter digest_result ~f:(fun digest ->
                if !Clflags.debug_digests
                then
                  Console.print
                    [ Pp.textf
                        "Re-digested file %s because its stats changed:"
                        (Path.to_string_maybe_quoted path)
                    ; Dyn.pp
                        (Dyn.Record
                           [ "old_digest", Digest.to_dyn x.digest
                           ; "new_digest", Digest.to_dyn digest
                           ; "old_stats", Reduced_stats.to_dyn x.stats
                           ; "new_stats", Reduced_stats.to_dyn reduced_stats
                           ])
                    ];
                needs_dumping := true;
                set_max_timestamp cache stats;
                x.digest <- digest;
                x.stats <- reduced_stats;
                x.stats_checked <- cache.checked_key);
              digest_result)))
;;

let peek_or_refresh_file ~allow_dirs path =
  match peek_file ~allow_dirs path with
  | Some digest_result -> digest_result
  | None -> refresh_without_removing_write_permissions ~allow_dirs path
;;

let build_file ~allow_dirs path = peek_or_refresh_file ~allow_dirs (Path.build path)

let remove path =
  let path = Path.build path in
  let cache = Lazy.force cache in
  needs_dumping := true;
  Path.Table.remove cache.table path
;;

module Untracked = struct
  let source_or_external_file = peek_or_refresh_file ~allow_dirs:false

  let invalidate_cached_timestamp path =
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
