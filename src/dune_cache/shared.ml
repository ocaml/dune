open Fiber.O
module Shared_cache_config = Config
module Store_result = Local.Store_result
module Restore_result = Local.Restore_result
module Digest_result = Dune_digest.Digest_result
module Artifacts = Local.Artifacts

let config = ref Config.Disabled

open Import

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

module Miss_reason = struct
  type t =
    | Cache_disabled
    | Cannot_go_in_shared_cache
    | Rerunning_for_reproducibility_check
    | Not_found_in_cache
    | Error of string

  let to_string = function
    | Cache_disabled -> "cache disabled"
    | Cannot_go_in_shared_cache -> "can't go in shared cache"
    | Error exn -> sprintf "error: %s" exn
    | Rerunning_for_reproducibility_check -> "rerunning for reproducibility check"
    | Not_found_in_cache -> "not found in cache"
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
      ~dir:(Lazy.force Layout.temp_dir)
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

let store_metadata ~mode ~rule_digest (artifacts : Digest.t Targets.Produced.t) =
  Targets.Produced.to_list_map artifacts ~f:(fun target digest ->
    { Artifacts.Metadata_entry.path = target; digest })
  |> Artifacts.Metadata_file.store ~mode ~rule_digest
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
        let path_in_cache = Lazy.force (Layout.file_path ~file_digest) in
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
    let result = store_metadata ~mode ~rule_digest artifacts in
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
      let src = Lazy.force (Layout.file_path ~file_digest) in
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

let try_to_restore_from_shared_cache ~mode ~rule_digest ~(targets : Targets.Validated.t)
  : (Digest.t Targets.Produced.t, Miss_reason.t) Hit_or_miss.t Fiber.t
  =
  let open Fiber.O in
  let+ () = Fiber.return () in
  match restore_artifacts ~mode ~rule_digest ~target_dir:targets.root with
  | Not_found_in_cache -> Hit_or_miss.Miss Miss_reason.Not_found_in_cache
  | Error exn -> Miss (Error (Printexc.to_string exn))
  | Restored artifacts ->
    (* it's a small departure from the general "debug cache" semantics that
       we're also printing successes, but it can be useful to see successes
       too if the goal is to understand when and how the file in the build
       directory appeared *)
    Dune_trace.emit ~buffered:true Cache (fun () ->
      let head = Targets.Validated.head targets in
      Dune_trace.Event.Cache.shared
        `Hit
        ~rule_digest:(Dune_digest.to_string rule_digest)
        ~head);
    Hit_or_miss.Hit artifacts
;;

let lookup_impl ~rule_digest ~targets =
  match !config with
  | Disabled -> Fiber.return (Hit_or_miss.Miss Miss_reason.Cache_disabled)
  | Enabled { storage_mode = mode; reproducibility_check } ->
    if Shared_cache_config.Reproducibility_check.sample reproducibility_check
    then
      (* CR-someday amokhov: Here we re-execute the rule, as in Jenga. To make
          [check_probability] more meaningful, we could first make sure that
          the shared cache actually does contain an entry for [rule_digest]. *)
      Fiber.return (Hit_or_miss.Miss Miss_reason.Rerunning_for_reproducibility_check)
    else try_to_restore_from_shared_cache ~mode ~rule_digest ~targets
;;

let lookup ~can_go_in_shared_cache ~rule_digest ~targets
  : Digest.t Targets.Produced.t option Fiber.t
  =
  let open Fiber.O in
  (if can_go_in_shared_cache
   then lookup_impl ~rule_digest ~targets
   else Fiber.return (Hit_or_miss.Miss Miss_reason.Cannot_go_in_shared_cache))
  >>| function
  | Hit result -> Some result
  | Miss (reason : Miss_reason.t) ->
    let always_emit =
      match reason with
      | Error _ | Rerunning_for_reproducibility_check -> true
      | _ -> false
    in
    let event () =
      let reason = Miss_reason.to_string reason in
      let head = Targets.Validated.head targets in
      let rule_digest = Dune_digest.to_string rule_digest in
      Dune_trace.Event.Cache.shared (`Miss reason) ~rule_digest ~head
    in
    if always_emit
    then Dune_trace.always_emit (event ())
    else Dune_trace.emit ~buffered:true Cache event;
    None
;;

(* If this function fails to store the rule to the shared cache, it returns
   [None] because we don't want this to be a catastrophic error. We simply log
   this incident and continue without saving the rule to the shared cache. *)
let try_to_store_to_shared_cache ~mode ~rule_digest ~action ~produced_targets
  : Digest.t Targets.Produced.t option Fiber.t
  =
  let open Fiber.O in
  let hex = Digest.to_string rule_digest in
  let pp_error msg =
    let action = action () in
    Pp.concat
      [ Pp.textf "cache store error [%s]: %s after executing" hex msg
      ; Pp.space
      ; Pp.char '('
      ; action
      ; Pp.char ')'
      ]
  in
  match
    Targets.Produced.map_with_errors
      produced_targets
      ~f:(fun target ->
        (* All of this monad boilerplate seems unnecessary since we
           don't care about errors... *)
        match Target.create target with
        | Some t -> Ok t
        | None -> Error ())
      ~d:(fun target ->
        match Target.create target with
        | Some _ -> Ok ()
        | None -> Error ())
  with
  | Error _ -> Fiber.return None
  | Ok targets ->
    store_artifacts ~mode ~rule_digest targets
    >>= (function
     | Stored targets_and_digests ->
       Log.info "cache store success" [ "hex", Dyn.string hex ];
       Fiber.return (Some targets_and_digests)
     | Already_present targets_and_digests ->
       Log.info "cache store skipped: already present" [ "hex", Dyn.string hex ];
       Fiber.return (Some targets_and_digests)
     | Error (Unix.Unix_error (Unix.EXDEV, "link", file)) ->
       (* We cannot hardlink across partitions so we kindly let the user know
            that they should use copy cache instead. *)
       Log.info
         "cache store error"
         [ "hex", Dyn.string hex
         ; "file", Dyn.string file
         ; "reason", Dyn.string "cannot link between file systems"
         ];
       Fiber.return None
     | Error exn ->
       Log.info "cache store error" [ "error", Dyn.string (Printexc.to_string exn) ];
       Fiber.return None
     | Will_not_store_due_to_non_determinism sexp ->
       (* CR-someday amokhov: We should systematically log all warnings. *)
       Log.info
         "cache store non-deterministic"
         [ "sexp", Dyn.string (Sexp.to_string sexp) ];
       User_warning.emit [ pp_error (Sexp.to_string sexp) ];
       Fiber.return None)
;;

module File_digest = struct
  module Digest_result = Dune_digest.Digest_result
  module Error = Digest_result.Error

  (* CR-soon rgrinberg: a bunch of this is duplicated from cached_digest.ml.
     This is temporary since [Cached_digest] is going to be limited source
     files *)

  let refresh_async ~allow_dirs stats path =
    let path = Path.build path in
    let open Fiber.O in
    Digest.Stats_for_digest.of_unix_stats stats
    |> Digest.path_with_stats_async ~allow_dirs path
    >>| function
    | Ok digest -> Ok digest
    | Error Unexpected_kind -> Error (Error.Unexpected_kind stats.st_kind)
    | Error (Unix_error (ENOENT, _, _)) -> Error No_such_file
    | Error (Unix_error other_error) -> Error (Unix_error other_error)
  ;;

  let refresh_without_removing_write_permissions_async ~allow_dirs path =
    match Unix.stat (Path.Build.to_string path) with
    | stats -> refresh_async stats ~allow_dirs path
    | exception exn ->
      Fiber.return
        (match exn with
         | Unix.Unix_error (ENOENT, _, _) ->
           (* Test if this is a broken symlink for better error messages. *)
           Digest_result.catch_fs_errors (fun () ->
             match Unix.lstat (Path.Build.to_string path) with
             | exception Unix.Unix_error (ENOENT, _, _) -> Error Error.No_such_file
             | _stats_so_must_be_a_symlink -> Error Broken_symlink)
         | exn -> Error (Digest_result.Error.of_exn exn))
  ;;

  let refresh_and_remove_write_permissions_async ~allow_dirs path =
    let open Digest_result.Error in
    match Unix.lstat (Path.Build.to_string path) with
    | exception Unix.Unix_error (ENOENT, _, _) -> Fiber.return (Error No_such_file)
    | exception exn -> Fiber.return (Error (Digest_result.Error.of_exn exn))
    | stats ->
      (match stats.st_kind with
       | S_LNK ->
         (match Unix.stat (Path.Build.to_string path) with
          | stats -> refresh_async stats ~allow_dirs:false path
          | exception Unix.Unix_error (ENOENT, _, _) ->
            Fiber.return (Error Broken_symlink)
          | exception exn -> Fiber.return (Error (Digest_result.Error.of_exn exn)))
       | S_REG ->
         let perm = Permissions.remove Permissions.write stats.st_perm in
         (match Unix.chmod (Path.Build.to_string path) perm with
          | () -> refresh_async ~allow_dirs:false { stats with st_perm = perm } path
          | exception exn -> Fiber.return (Error (Digest_result.Error.of_exn exn)))
       | _ ->
         (* CR-someday amokhov: Shall we proceed if [stats.st_kind = S_DIR]?
          What about stranger kinds like [S_SOCK]? *)
         refresh_async ~allow_dirs stats path)
  ;;

  let refresh ~allow_dirs ~remove_write_permissions path =
    (if remove_write_permissions
     then refresh_and_remove_write_permissions_async
     else refresh_without_removing_write_permissions_async)
      ~allow_dirs
      path
  ;;
end

let compute_target_digests_or_raise_error
      ~should_remove_write_permissions_on_generated_files
      ~loc
      ~produced_targets
  : Digest.t Targets.Produced.t Fiber.t
  =
  let open Fiber.O in
  let* () = Fiber.return () in
  let compute_digest =
    (* Remove write permissions on targets. A first theoretical reason is that
       the build process should be a computational graph and targets should
       not change state once built. A very practical reason is that enabling
       the cache will remove write permission because of hardlink sharing
       anyway, so always removing them enables to catch mistakes earlier. *)
    File_digest.refresh
      ~allow_dirs:true
      ~remove_write_permissions:should_remove_write_permissions_on_generated_files
  in
  Targets.Produced.map_with_errors_fiber ~f:compute_digest produced_targets
  >>| function
  | Ok result -> result
  | Error errors ->
    let missing, errors =
      let process_target (target, error) =
        if Digest_result.Error.no_such_file error
        then Left target
        else (
          let error = Digest_result.Error.pp error (Path.build target) in
          Right (target, error))
      in
      Nonempty_list.to_list errors |> List.partition_map ~f:process_target
    in
    (match missing, errors with
     | [], [] ->
       (* This is impossible because [errors] is non-empty and [List.partition_map]
          on a non empty list should also return at least one least that isn't empty.
          Unfortunately, the type of such a [Nonempty_list.partition_map] would be
          rather awkward. *)
       Code_error.raise
         "compute_target_digests_or_raise_error: this is impossible because we should at \
          least be showing the original error"
         [ "targets", Targets.Produced.to_dyn produced_targets ]
     | missing, errors ->
       User_error.raise
         ~loc
         ((match missing with
           | [] -> []
           | _ ->
             [ Pp.textf "Rule failed to generate the following targets:"
             ; List.sort missing ~compare:Path.Build.compare
               |> List.map ~f:Path.build
               |> Pp.enumerate ~f:Path.pp
             ])
          @
          match errors with
          | [] -> []
          | _ ->
            [ Pp.textf "Error trying to read targets after a rule was run:"
            ; List.sort errors ~compare:(fun x y -> Path.Build.compare (fst x) (fst y))
              |> Pp.enumerate ~f:(fun (target, error) ->
                Pp.concat ~sep:(Pp.verbatim ": ") [ Path.pp (Path.build target); error ])
            ]))
;;

let examine_targets_and_store
      ~can_go_in_shared_cache
      ~loc
      ~rule_digest
      ~should_remove_write_permissions_on_generated_files
      ~action
      ~(produced_targets : unit Targets.Produced.t)
  : Digest.t Targets.Produced.t Fiber.t
  =
  match !config with
  | Enabled { storage_mode = mode; reproducibility_check = _ } when can_go_in_shared_cache
    ->
    let open Fiber.O in
    try_to_store_to_shared_cache ~mode ~rule_digest ~produced_targets ~action
    >>= (function
     | Some produced_targets_with_digests -> Fiber.return produced_targets_with_digests
     | None ->
       compute_target_digests_or_raise_error
         ~should_remove_write_permissions_on_generated_files
         ~loc
         ~produced_targets)
  | _ ->
    compute_target_digests_or_raise_error
      ~should_remove_write_permissions_on_generated_files
      ~loc
      ~produced_targets
;;
