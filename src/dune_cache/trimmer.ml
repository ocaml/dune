open Import

module Trimming_result = struct
  type t =
    { trimmed_bytes : int64
    ; number_of_files_removed : int
    }

  let empty = { trimmed_bytes = 0L; number_of_files_removed = 0 }

  (* CR-someday amokhov: Right now Dune doesn't support large (>1Gb) files on
     32-bit platforms due to the pervasive use of [int] for representing
     individual file sizes. It's not fundamentally difficult to switch to
     [int64], so we should do it if it becomes a real issue. *)
  let add t ~(bytes : int) =
    { trimmed_bytes = Int64.add t.trimmed_bytes (Int64.of_int bytes)
    ; number_of_files_removed = t.number_of_files_removed + 1
    }
  ;;
end

let trim_broken_metadata_entries ~trimmed_so_far =
  List.fold_left
    Version.Metadata.all
    ~init:trimmed_so_far
    ~f:(fun trimmed_so_far version ->
      let file_path =
        Layout.Versioned.file_path (Version.Metadata.file_version version)
      in
      Layout.Versioned.list_metadata_entries version
      |> Lazy.force
      |> List.fold_left
           ~init:trimmed_so_far
           ~f:(fun trimmed_so_far (path, rule_or_action_digest) ->
             let should_be_removed =
               match Local.Metadata.Versioned.restore version ~rule_or_action_digest with
               | Not_found_in_cache ->
                 (* A concurrent process must have removed this metadata file. No
                 need to try removing such "phantom" metadata files again. *)
                 false
               | Error _exn ->
                 (* If a metadata file can't be restored, let's trim it. *)
                 true
               | Restored metadata ->
                 (match metadata with
                  | Value _ ->
                    (* We do not expect to see any value entries in the cache. Let's
                    keep them untrimmed for now. *)
                    false
                  | Artifacts { entries; metadata = _ } ->
                    List.exists entries ~f:(function
                      | { Local.Artifacts.Metadata_entry.digest = None; path = _ } ->
                        (* no digest means it's a directory. *)
                        false
                      | { digest = Some file_digest; path = _ } ->
                        let reference = Lazy.force (file_path ~file_digest) in
                        not (Fpath.exists (Path.to_string reference))))
             in
             match should_be_removed with
             | false -> trimmed_so_far
             | true ->
               (match Path.stat path with
                | Error _ ->
                  (* Alas, here we can't take any (non-zero) credit, since we don't
                  know the size of the deleted file. *)
                  trimmed_so_far
                | Ok stats ->
                  let bytes = stats.st_size in
                  (* If another process deletes [path] and the [unlink_no_err] below
                     is a no-op, we take the credit and increase
                     [trimmed_so_far]. *)
                  Fpath.unlink_no_err (Path.to_string path);
                  Trimming_result.add trimmed_so_far ~bytes)))
;;

let garbage_collect () =
  trim_broken_metadata_entries ~trimmed_so_far:Trimming_result.empty
;;

let files_in_cache_for_all_supported_versions () =
  List.concat_map Version.File.all ~f:(fun file_version ->
    Lazy.force (Layout.Versioned.list_file_entries file_version))
;;

(* We call a cached file "unused" if there are currently no hard links to it
   from build directories. Note that [st_nlink] can return 0 if the file has
   been removed since we scanned the tree -- in this case we do not want to
   claim that its removal is the result of cache trimming and we, therefore,
   skip it while trimming. *)
let file_exists_and_is_unused ~stats = stats.Unix.st_nlink = 1

(* Dune uses [ctime] to prioritise entries for deletion. How does this work?

   - In the [Hardlink] mode, an entry to become unused when it loses the last
     hard link that points to it from a build directory. When this happens, the
     entry's [ctime] is modified. This means that the trimmer will start deleting
     entries starting from the one that became unused first.

   - In the [Copy] mode, all entries have hard link count of 1, and so they all
     appear to be unused to the trimmer. However, copying an entry to the cache,
     as well as copying it from the cache to a build directory, both change the
     entry's [ctime]. This means that the trimmer will start deleting entries
     starting from the one that was least recently created or used. *)
let trim ~goal =
  let files =
    files_in_cache_for_all_supported_versions ()
    |> List.filter_map ~f:(fun (path, _) ->
      match Path.stat path with
      | Error _ -> None
      | Ok stats ->
        if file_exists_and_is_unused ~stats
        then Some (path, stats.st_size, stats.st_ctime)
        else None)
    |> List.sort ~compare:(fun (_, _, ctime1) (_, _, ctime2) ->
      Float.compare ctime1 ctime2)
  in
  let delete (trimmed_so_far : Trimming_result.t) (path, bytes, _) =
    if trimmed_so_far.trimmed_bytes >= goal
    then trimmed_so_far
    else (
      Fpath.unlink_no_err (Path.to_string path);
      (* CR-someday amokhov: We should really be using block_size * #blocks
         because that's how much we save actually. *)
      Trimming_result.add trimmed_so_far ~bytes)
  in
  let trimmed_so_far = List.fold_left ~init:Trimming_result.empty ~f:delete files in
  trim_broken_metadata_entries ~trimmed_so_far
;;

let overhead_size () =
  files_in_cache_for_all_supported_versions ()
  |> List.fold_left ~init:0L ~f:(fun acc (p, _) ->
    let size =
      try
        let stats = Unix.stat (Path.to_string p) in
        if file_exists_and_is_unused ~stats then Int64.of_int stats.st_size else 0L
      with
      | Unix.Unix_error (Unix.ENOENT, _, _) -> 0L
    in
    Int64.add acc size)
;;

let clear () =
  let rm_rf path = Path.rm_rf ~allow_external:true path in
  let rmdir path =
    try Unix.rmdir (Path.to_string path) with
    | Unix.Unix_error ((ENOENT | ENOTEMPTY), _, _) -> ()
  in
  let rm_rf_all versions dir =
    List.iter versions ~f:(fun version ->
      let dir = Lazy.force (dir version) in
      rm_rf dir;
      Option.iter ~f:rmdir (Path.parent dir))
  in
  rm_rf_all Version.Metadata.all Layout.Versioned.metadata_storage_dir;
  rm_rf_all Version.File.all Layout.Versioned.file_storage_dir;
  rm_rf (Lazy.force Layout.temp_dir);
  (* Do not catch errors when deleting the root directory so that they are
     reported to the user. *)
  Layout.build_cache_dir |> Lazy.force |> Path.to_string |> Unix.rmdir
;;
