open Stdune
open Dune_cache_storage

module Trimming_result = struct
  type t = { trimmed_bytes : int64 }

  let empty = { trimmed_bytes = 0L }

  (* CR-someday amokhov: Right now Dune doesn't support large (>1Gb) files on
     32-bit platforms due to the pervasive use of [int] for representing
     individual file sizes. It's not fundamentally difficult to switch to
     [int64], so we should do it if it becomes a real issue. *)
  let add t ~(bytes : int) =
    { trimmed_bytes = Int64.add t.trimmed_bytes (Int64.of_int bytes) }
end

let trim_broken_metadata_entries ~trimmed_so_far =
  List.fold_left Version.Metadata.all ~init:trimmed_so_far
    ~f:(fun trimmed_so_far version ->
      let metadata_entries = Layout.Versioned.list_metadata_entries version in
      let file_path =
        Layout.Versioned.file_path (Version.Metadata.file_version version)
      in
      List.fold_left metadata_entries ~init:trimmed_so_far
        ~f:(fun trimmed_so_far (path, rule_or_action_digest) ->
          let should_be_removed =
            match Metadata.Versioned.restore version ~rule_or_action_digest with
            | Not_found_in_cache ->
              (* A concurrent process must have removed this metadata file. No
                 need to try removing such "phantom" metadata files again. *)
              false
            | Error _exn ->
              (* If a metadata file can't be restored, let's trim it. *)
              true
            | Restored metadata -> (
              match metadata with
              | Metadata.Value _ ->
                (* We do not expect to see any value entries in the cache. Let's
                   keep them untrimmed for now. *)
                false
              | Metadata.Artifacts { entries; _ } ->
                List.exists entries
                  ~f:(fun { Artifacts.Metadata_entry.file_digest; _ } ->
                    let reference = file_path ~file_digest in
                    not (Path.exists reference)))
          in
          match should_be_removed with
          | true ->
            let bytes = (Path.stat path).st_size in
            Path.unlink_no_err path;
            Trimming_result.add trimmed_so_far ~bytes
          | false -> trimmed_so_far))

let garbage_collect () =
  trim_broken_metadata_entries ~trimmed_so_far:Trimming_result.empty

let files_in_cache_for_all_supported_versions () =
  List.concat_map Version.File.all ~f:(fun file_version ->
      Layout.Versioned.list_file_entries file_version)

(* We call a cached file "unused" if there are currently no hard links to it
   from build directories. Note that [st_nlink] can return 0 if the file has
   been removed since we scanned the tree -- in this case we do not want to
   claim that its removal is the result of cache trimming and we, therefore,
   skip it while trimming. *)
let file_exists_and_is_unused ~stats = stats.Unix.st_nlink = 1

let trim ~goal =
  let files = files_in_cache_for_all_supported_versions () |> List.map ~f:fst in
  let f path =
    let stats = Path.stat path in
    if file_exists_and_is_unused ~stats then
      Some (path, stats.st_size, stats.st_ctime)
    else
      None
  and compare (_, _, t1) (_, _, t2) = Poly.compare t1 t2 in
  let files = List.sort ~compare (List.filter_map ~f files)
  and delete (trimmed_so_far : Trimming_result.t) (path, bytes, _) =
    if trimmed_so_far.trimmed_bytes >= goal then
      trimmed_so_far
    else (
      Path.unlink path;
      (* CR-someday amokhov: We should really be using block_size * #blocks
         because that's how much we save actually. *)
      Trimming_result.add trimmed_so_far ~bytes
    )
  in
  let trimmed_so_far =
    List.fold_left ~init:Trimming_result.empty ~f:delete files
  in
  trim_broken_metadata_entries ~trimmed_so_far

let overhead_size () =
  let files = files_in_cache_for_all_supported_versions () |> List.map ~f:fst in
  let stats =
    let f p =
      try
        let stats = Path.stat p in
        if file_exists_and_is_unused ~stats then
          Int64.of_int stats.st_size
        else
          0L
      with
      | Unix.Unix_error (Unix.ENOENT, _, _) -> 0L
    in
    List.map ~f files
  in
  List.fold_left ~f:Int64.add ~init:0L stats
