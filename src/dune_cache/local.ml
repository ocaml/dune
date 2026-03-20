open Import
open Layout
open Fiber.O

(* See [doc/dev/cache.md] for design and implementation notes. *)

module Store_result = struct
  type t =
    | Stored
    | Already_present
    | Error of exn
    | Will_not_store_due_to_non_determinism of Sexp.t

  let combine x y =
    match x, y with
    | Will_not_store_due_to_non_determinism details, _ ->
      Will_not_store_due_to_non_determinism details
    | _, Will_not_store_due_to_non_determinism details ->
      Will_not_store_due_to_non_determinism details
    | Error e, _ -> Error e
    | _, Error e -> Error e
    | Stored, _ -> Stored
    | _, Stored -> Stored
    | Already_present, Already_present -> Already_present
  ;;

  let empty = Already_present
end

module Restore_result = struct
  type 'data t =
    | Restored of 'data
    | Not_found_in_cache
    | Error of exn

  let bind t ~f =
    match t with
    | Restored data -> f data
    | (Not_found_in_cache | Error _) as res -> res
  ;;

  let map t ~f =
    match t with
    | Restored data -> Restored (f data)
    | (Not_found_in_cache | Error _) as res -> res
  ;;
end

let restore_file_content path : string Restore_result.t =
  match Io.read_file ~binary:false path with
  | contents -> Restored contents
  | exception Sys_error (_some_error_message : string) ->
    (* CR-someday amokhov: [Io.read_file] doesn't raise "typed" exceptions like
       [Unix_error], so we guess here that the exception means "file not found".
       Can we make the API of [Io] more precise? *)
    Not_found_in_cache
  | exception e ->
    (* This code path might be unreachable until the above is resolved. *)
    Error e
;;

module Matches_existing_query = struct
  type t =
    | Match
    | Mismatch of Sexp.t
end

(* Store [metadata] corresponding to a given [rule_or_action_digest] to the
   cache using the supplied [to_sexp] serializer. If the cache already contains
   an entry for the hash, we use [matches_existing_entry] to check that the
   given [content] matches the previously stored one. If this is not the case,
   we return [Will_not_store_due_to_non_determinism]. *)
let store_metadata ~mode ~rule_or_action_digest ~metadata ~to_sexp ~matches_existing_entry
  : Store_result.t
  =
  let content = Csexp.to_string (to_sexp metadata) in
  let path_in_cache = Lazy.force (Layout.metadata_path ~rule_or_action_digest) in
  match Util.write_atomically ~mode ~content path_in_cache with
  | Ok -> Stored
  | Error e -> Error e
  | Already_present ->
    (match restore_file_content path_in_cache with
     | Not_found_in_cache ->
       (* This can actually happen, but we think it's an unlikely case. The
          [Already_present] branch should already be rarely visited (only if
          multiple build systems attempt to store the same entry), but also a
          trimmer must be running in parallel to delete this file. *)
       Error (Failure "Race in store_metadata")
     | Error e -> Error e
     | Restored existing_content ->
       (match
          (matches_existing_entry metadata ~existing_content : Matches_existing_query.t)
        with
        | Mismatch details -> Will_not_store_due_to_non_determinism details
        | Match ->
          (* At this point we could in principle overwrite the existing metadata
             file with the new [content] because it seems fresher. We choose not
             to do that because in practice we end up here only due to racing. The
             racing processes are not totally ordered, so neither content is
             really fresher than the other. *)
          Already_present))
;;

let restore_metadata_file file ~of_sexp : _ Restore_result.t =
  restore_file_content file
  |> Restore_result.bind ~f:(fun content ->
    match Csexp.parse_string content with
    | Error (_offset, msg) -> Error (Failure msg)
    | Ok sexp ->
      (match of_sexp sexp with
       | Ok content -> Restored content
       | Error e -> Error e))
;;

(* Read a metadata file corresponding to a given [rule_or_action_digest] from
   the cache and parse it using the supplied [of_sexp] parser. *)
let restore_metadata ~rule_or_action_digest ~of_sexp : _ Restore_result.t =
  Layout.metadata_path ~rule_or_action_digest
  |> Lazy.force
  |> restore_metadata_file ~of_sexp
;;

type metadata = Sexp.t list

module Value = struct
  module Metadata_file = struct
    type t =
      { metadata : metadata
      ; value_digest : Digest.t
      }

    let of_sexp = function
      | Sexp.List
          [ List (Atom "metadata" :: metadata)
          ; List [ Atom "value"; Sexp.Atom value_hash ]
          ] ->
        (match Digest.from_hex value_hash with
         | Some value_digest -> Ok { metadata; value_digest }
         | None -> Error (Failure "Cannot parse cache metadata: malformed value digest"))
      | _ -> Error (Failure "Cannot parse cache metadata")
    ;;
  end
end

module Artifacts = struct
  module Metadata_entry = struct
    type t =
      { path : Path.Local.t
      ; digest : Digest.t option
      }

    let equal x y =
      Path.Local.equal x.path y.path && Option.equal Digest.equal x.digest y.digest
    ;;

    let digest_to_sexp = function
      | None -> Sexp.Atom "<dir>"
      | Some digest -> Sexp.Atom (Digest.to_string digest)
    ;;

    let to_sexp { path; digest } =
      Sexp.List [ Atom (Path.Local.to_string path); digest_to_sexp digest ]
    ;;

    let digest_of_sexp = function
      | "<dir>" -> Ok None
      | digest ->
        (match Digest.from_hex digest with
         | Some digest -> Ok (Some digest)
         | None ->
           Error
             (Failure
                (sprintf "Cannot parse file digest %S in cache metadata entry" digest)))
    ;;

    let of_sexp = function
      | Sexp.List [ Atom path; Atom digest ] ->
        (match digest_of_sexp digest with
         | Ok digest -> Ok { path = Path.Local.of_string path; digest }
         | Error e -> Error e)
      | _ -> Error (Failure "Cannot parse cache metadata entry")
    ;;
  end

  module Metadata_file = struct
    type t =
      { metadata : Sexp.t list
      ; (* The entries are listed in the same order that they were provided when
           storing artifacts in the cache. We keep the order to avoid confusion
           even though sorting the entres is tempting. *)
        entries : Metadata_entry.t list
      }

    let to_sexp { metadata; entries } =
      Sexp.List
        [ List (Atom "metadata" :: metadata)
        ; List (Atom "files" :: List.map entries ~f:Metadata_entry.to_sexp)
        ]
    ;;

    let of_sexp = function
      | Sexp.List [ List (Atom "metadata" :: metadata); List (Atom "files" :: entries) ]
        ->
        let entries = List.map entries ~f:Metadata_entry.of_sexp in
        (match Result.List.all entries with
         | Ok entries -> Ok { metadata; entries }
         | Error e -> Error e)
      | _ -> Error (Failure "Cannot parse cache metadata")
    ;;

    let matches_existing_entry t ~existing_content : Matches_existing_query.t =
      match Csexp.parse_string existing_content with
      | Error _ -> Mismatch (Atom "Malformed value in cache")
      | Ok sexp ->
        (match of_sexp sexp with
         | Error _ -> Mismatch (Atom "Malformed value in cache")
         | Ok existing ->
           (match List.equal Metadata_entry.equal t.entries existing.entries with
            | true -> Match
            | false ->
              Mismatch
                (Sexp.record
                   [ ( "in_cache"
                     , Sexp.List (List.map ~f:Metadata_entry.to_sexp existing.entries) )
                   ; "computed", Sexp.List (List.map ~f:Metadata_entry.to_sexp t.entries)
                   ])))
    ;;

    let store t ~mode ~rule_digest =
      store_metadata
        ~mode
        ~rule_or_action_digest:rule_digest
        ~metadata:t
        ~to_sexp
        ~matches_existing_entry
    ;;

    let restore ~rule_digest =
      restore_metadata ~rule_or_action_digest:rule_digest ~of_sexp
    ;;
  end

  let list ~rule_digest =
    Metadata_file.restore ~rule_digest
    |> Restore_result.map ~f:(fun ({ entries; _ } : Metadata_file.t) -> entries)
  ;;
end

module Metadata = struct
  type t =
    | Artifacts of Artifacts.Metadata_file.t
    | Value of Value.Metadata_file.t

  let of_sexp sexp : (t, exn) result =
    match Artifacts.Metadata_file.of_sexp sexp with
    | Ok res -> Ok (Artifacts res)
    | Error _exn ->
      (* CR-someday amokhov: Here we are discarding the [_exn] but it may be
         better to combine the two exceptions when both parsers fail. *)
      Value.Metadata_file.of_sexp sexp |> Result.map ~f:(fun res -> Value res)
  ;;

  let restore ~metadata_path ~rule_or_action_digest =
    metadata_path ~rule_or_action_digest |> Lazy.force |> restore_metadata_file ~of_sexp
  ;;

  module Versioned = struct
    let restore version = restore ~metadata_path:(Layout.Versioned.metadata_path version)
  end
end

module Store_artifacts_result = struct
  type t =
    | Stored of Digest.t Targets.Produced.t
    | Already_present of Digest.t Targets.Produced.t
    | Error of exn
    | Will_not_store_due_to_non_determinism of Sexp.t

  let of_store_result ~artifacts t =
    match (t : Store_result.t) with
    | Stored -> Stored artifacts
    | Already_present -> Already_present artifacts
    | Error exn -> Error exn
    | Will_not_store_due_to_non_determinism details ->
      Will_not_store_due_to_non_determinism details
  ;;

  let bind t ~f =
    match t with
    | Stored data -> f data
    | Already_present data -> f data
    | (Error _ | Will_not_store_due_to_non_determinism _) as res -> res
  ;;
end

module Target = struct
  type t =
    | File of { executable : bool }
    | Directory

  let executable = function
    | File { executable } -> executable
    | Directory -> Code_error.raise "Target.executable called on directory target" []
  ;;

  let create path =
    let path = Path.Build.to_string path in
    match Unix.lstat path with
    | { Unix.st_kind = Unix.S_REG; st_perm; _ } ->
      Unix.chmod path (Permissions.remove Permissions.write st_perm);
      let executable = Permissions.test Permissions.execute st_perm in
      Some (File { executable })
    | { Unix.st_kind = Unix.S_DIR; st_perm; _ } ->
      (* Adding "executable" permissions to directories mean we can traverse them. *)
      Unix.chmod path (Permissions.add Permissions.execute st_perm);
      Some Directory
    | (exception Unix.Unix_error _) | _ -> None
  ;;
end

(* This function is like [Unix.link] but handles the "Too many links" error by
   creating a copy of the [src] in a temporary directory, then atomically
   replacing the [src] with the copy, and finally creating the requested [dst]
   by calling [Unix.link src dst] again.

   We hit the "Too many links" error because we store a lot of empty files in
   the cache, which all get deduplicated into the same cache entry. This
   function essentially deletes the "overlinked" entry from the cache, creating
   a fresh copy with the 0 link count. This leads to some duplication but it's
   negligible: we might store the empty file several times across all workspaces
   instead of just storing it once.

   If you need to debug this function, you can trigger the "Too many links"
   error by running [for i in {1..100000}; do ln $file tmp/$i; done], where the
   [$file] is the shared cache entry for the empty file. After that, no more
   hard links on [$file] will be allowed, triggering the [EMLINK] code path. *)
let link_even_if_there_are_too_many_links_already ~src ~dst =
  try Fpath.link (Path.to_string src) (Path.to_string dst) with
  | Unix.Unix_error (Unix.EMLINK, _, _) ->
    Temp.with_temp_file
      ~dir:(Lazy.force temp_dir)
      ~prefix:"dune"
      ~suffix:"copy"
      ~f:(function
      | Error e -> raise e
      | Ok temp_file ->
        Io.copy_file ~src ~dst:temp_file ();
        (* This replaces [src], which has too many links already, with a fresh
           copy we've just created in the [temp_file]. *)
        let src = Path.to_string src in
        Unix.rename (Path.to_string temp_file) src;
        (* This should now succeed. *)
        Fpath.link src (Path.to_string dst))
;;

let store_metadata ~mode ~metadata ~rule_digest (artifacts : Digest.t Targets.Produced.t) =
  let entries =
    Targets.Produced.to_list_map artifacts ~f:(fun target digest ->
      { Artifacts.Metadata_entry.path = target; digest })
  in
  Artifacts.Metadata_file.store ~mode { metadata; entries } ~rule_digest
;;

(* Step I of [store_skipping_metadata].

     If any of the targets couldn't be stored in the temporary directory, then
     the result is [Error] with the corresponding exception. Otherwise, the
     result is [Ok ()]. *)
let store_targets_to ~temp_dir ~(targets : _ Targets.Produced.t) ~mode : unit Or_exn.t =
  let portable_hardlink_or_copy =
    match (mode : Mode.t) with
    | Hardlink -> Io.portable_hardlink
    | Copy -> fun ~src ~dst -> Io.copy_file ~src ~dst ()
  in
  Result.try_with (fun () ->
    (* CR-someday rleshchinskiy: We recreate the directory structure here but it might be
         simpler to just use file digests instead of file names and no subdirectories. *)
    (* The comment above seems outdated wrt. 'no subdirectories'... *)
    Targets.Produced.iteri
      targets
      ~d:(fun dir -> Path.mkdir_p (Path.append_local temp_dir dir))
      ~f:(fun file _ ->
        let path_in_build_dir = Path.build (Path.Build.append_local targets.root file) in
        let path_in_temp_dir = Path.append_local temp_dir file in
        portable_hardlink_or_copy ~src:path_in_build_dir ~dst:path_in_temp_dir))
;;

(* Step II of [store_skipping_metadata].

     Computing digests can be slow, so we do that in parallel. *)
let compute_digests_in ~temp_dir ~targets : Digest.t Targets.Produced.t Or_exn.t Fiber.t =
  let open Fiber.O in
  Fiber.collect_errors (fun () ->
    Targets.Produced.parallel_map targets ~f:(fun path target ->
      let executable = Target.executable target in
      let file = Path.append_local temp_dir path in
      Dune_digest.file_with_executable_bit ~executable file))
  >>| Result.map_error ~f:(function
    | exn :: _ -> exn.Exn_with_backtrace.exn
    | [] -> assert false)
;;

(* Step III of [store_skipping_metadata]. *)
let store_to_cache_from ~temp_dir ~mode artifacts =
  Targets.Produced.foldi
    artifacts
    ~init:Store_result.empty
    ~f:(fun target digest results ->
      match digest with
      | None ->
        (* No digest means [target] is a directory, simply ignore it. *)
        results
      | Some file_digest ->
        let path_in_temp_dir = Path.append_local temp_dir target in
        let path_in_cache = Lazy.force (file_path ~file_digest) in
        let store_using_hardlinks () =
          match Util.Optimistically.link ~src:path_in_temp_dir ~dst:path_in_cache with
          | exception Unix.Unix_error (Unix.EEXIST, _, _) ->
            (* We end up here if the cache already contains an entry for this
                 artifact. We deduplicate by keeping only one copy, in the
                 cache. *)
            let path_in_build_dir =
              Path.build (Path.Build.append_local artifacts.root target)
            in
            (match
               Fpath.unlink_no_err (Path.to_string path_in_temp_dir);
               (* At first, we deduplicate the temporary file. Doing this
                    intermediate step allows us to keep the original target in case
                    the below link step fails. This might happen if the trimmer has
                    just deleted [path_in_cache]. In this rare case, this function
                    fails with an [Error], and so we might end up with some
                    duplicates in the workspace. *)
               link_even_if_there_are_too_many_links_already
                 ~src:path_in_cache
                 ~dst:path_in_temp_dir;
               (* Now we can simply rename the temporary file into the target,
                    knowing that the original target remains in place if the
                    renaming fails.

                    One curious case to think about is if the file in the cache
                    happens to have the same inode as the file in the workspace. In
                    that case this deduplication should be a no-op, but the
                    [rename] operation has a quirk where [path_in_temp_dir] can
                    remain on disk. This is not a problem because we clean the
                    temporary directory later. *)
               Fpath.rename_exn
                 (Path.to_string path_in_temp_dir)
                 (Path.to_string path_in_build_dir)
             with
             | exception e -> Store_result.Error e
             | () -> Already_present)
          | exception e -> Error e
          | () -> Stored
        in
        let store_using_test_and_rename () =
          (* CR-someday amokhov: There is a race here. If [path_in_cache] is
               created after [Fpath.exists] but before [Path.rename], it will be
               silently overwritten. Find a good way to avoid this race. *)
          match Fpath.exists (Path.to_string path_in_cache) with
          | true -> Store_result.Already_present
          | false ->
            (match
               Util.Optimistically.rename ~src:path_in_temp_dir ~dst:path_in_cache
             with
             | exception e -> Error e
             | () -> Stored)
        in
        let result =
          match (mode : Mode.t) with
          | Hardlink -> store_using_hardlinks ()
          | Copy -> store_using_test_and_rename ()
        in
        Store_result.combine results result)
;;

let store_skipping_metadata ~mode ~targets : Store_artifacts_result.t Fiber.t =
  Fiber.Temp.with_temp_dir
    ~parent_dir:(Lazy.force Layout.temp_dir)
    ~prefix:"dune"
    ~suffix:"artifacts"
    ~f:(function
    | Error exn -> Fiber.return (Store_artifacts_result.Error exn)
    | Ok temp_dir ->
      (match store_targets_to ~temp_dir ~targets ~mode with
       | Error exn -> Fiber.return (Store_artifacts_result.Error exn)
       | Ok () ->
         compute_digests_in ~temp_dir ~targets
         >>| (function
          | Error exn -> Store_artifacts_result.Error exn
          | Ok artifacts ->
            let result = store_to_cache_from ~temp_dir ~mode artifacts in
            Store_artifacts_result.of_store_result ~artifacts result)))
;;

let store_artifacts ~mode ~rule_digest targets : Store_artifacts_result.t Fiber.t =
  let+ result = store_skipping_metadata ~mode ~targets in
  Store_artifacts_result.bind result ~f:(fun artifacts ->
    let result = store_metadata ~mode ~rule_digest ~metadata:[] artifacts in
    Store_artifacts_result.of_store_result ~artifacts result)
;;

module File_restore = struct
  exception E of Digest.t Targets.Produced.t Restore_result.t

  module Unwind : sig
    type t

    val make : unit -> t
    val push : t -> (unit -> unit) -> unit
    val unwind : t -> unit
  end = struct
    type t = (unit -> unit) list ref

    let make () = ref []
    let push t f = t := f :: !t

    let unwind t =
      List.iter !t ~f:(fun f ->
        try f () with
        | _ -> ());
      t := []
    ;;
  end

  let hardlink ~src ~dst =
    try link_even_if_there_are_too_many_links_already ~src ~dst with
    | Unix.Unix_error (Unix.ENOENT, _, _) -> raise_notrace (E Not_found_in_cache)
    | exn -> raise_notrace (E (Error exn))
  ;;

  let copy ~src ~dst =
    try Io.copy_file ~src ~dst () with
    | Sys_error _ -> raise_notrace (E Not_found_in_cache)
  ;;

  let create_all_or_none (mode : Mode.t) (artifacts : _ Targets.Produced.t) =
    let unwind = Unwind.make () in
    let rec mk_dir (dir : Path.Local.t) =
      (match Path.Local.parent dir with
       | Some parent when not (Path.Local.is_root parent) -> mk_dir parent
       | Some _ | None -> ());
      let path = Path.build (Path.Build.append_local artifacts.root dir) in
      if not (Fpath.exists (Path.to_string path))
      then (
        Path.mkdir_p path;
        Unwind.push unwind (fun () -> Unix.rmdir (Path.to_string path)))
    in
    let mk_file file file_digest =
      let target = Path.Build.append_local artifacts.root file in
      let dst = Path.build target in
      let src = Lazy.force (file_path ~file_digest) in
      (match mode with
       | Hardlink -> hardlink ~src ~dst
       | Copy -> copy ~src ~dst);
      Unwind.push unwind (fun () -> Fpath.unlink_no_err (Path.Build.to_string target))
    in
    try Targets.Produced.iteri artifacts ~f:mk_file ~d:mk_dir with
    | exn ->
      Unwind.unwind unwind;
      reraise exn
  ;;
end

let restore_artifacts ~mode ~rule_digest ~target_dir =
  Artifacts.list ~rule_digest
  |> Restore_result.bind ~f:(fun (entries : Artifacts.Metadata_entry.t list) ->
    let artifacts =
      Path.Local.Map.of_list_map_exn
        entries
        ~f:(fun { Artifacts.Metadata_entry.path; digest } -> path, digest)
      |> Targets.Produced.of_files target_dir
    in
    try
      File_restore.create_all_or_none mode artifacts;
      Restored artifacts
    with
    | File_restore.E result ->
      (* If [result] is [Not_found_in_cache] then one of the entries mentioned in
           the metadata is missing. The trimmer will eventually delete such "broken"
           metadata, so it is reasonable to consider that this [rule_digest] is not
           found in the cache. *)
      result)
;;
