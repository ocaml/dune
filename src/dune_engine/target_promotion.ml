open! Import

(* [To_delete] is used mostly to implement [dune clean]. It is an imperfect
   heuristic, in particular it can go wrong if:

   - the user deletes .to-delete-in-source-tree file

   - the user edits a previously promoted file with the intention of keeping it
   in the source tree, or creates a new file with the same name. *)
module To_delete = struct
  module P = Dune_util.Persistent.Make (struct
    (* CR-someday amokhov: This should really be a [Path.Source.Set.t] but
       changing it now would require bumping the [version]. Should we do it? *)
    type t = Path.Set.t

    let name = "PROMOTED-TO-DELETE"

    let version = 1

    let to_dyn = Path.Set.to_dyn
  end)

  let fn = Path.relative Path.build_dir ".to-delete-in-source-tree"

  (* [db] is used to accumulate promoted files from rules. *)
  let db = lazy (ref (Option.value ~default:Path.Set.empty (P.load fn)))

  let get_db () = !(Lazy.force db)

  let set_db new_db = Lazy.force db := new_db

  let needs_dumping = ref false

  let modify_db f =
    match f (get_db ()) with
    | None -> ()
    | Some new_db ->
      set_db new_db;
      needs_dumping := true

  let add p =
    let p = Path.source p in
    modify_db (fun db ->
        if Path.Set.mem db p then None else Some (Path.Set.add db p))

  let remove p =
    let p = Path.source p in
    modify_db (fun db ->
        if Path.Set.mem db p then Some (Path.Set.remove db p) else None)

  let dump () =
    if !needs_dumping && Path.build_dir_exists () then (
      needs_dumping := false;
      get_db () |> P.dump fn)

  let mem p =
    let p = Path.source p in
    Path.Set.mem !(Lazy.force db) p

  let () = Hooks.End_of_build.always dump
end

let files_in_source_tree_to_delete () = To_delete.get_db ()

(* TODO: Delete this step after users of Dune <2.8 are sufficiently rare. This
   step is sketchy because it's using the [To_delete] database and that can get
   out of date (see a comment on [To_delete]), so we should not widen the scope
   of it too much. *)
let delete_stale_dot_merlin_file ~dir ~source_files_to_ignore =
  (* If a [.merlin] file is present in the [To_delete] set but not in the
     [Source_files_to_ignore] that means the rule that ordered its promotion is
     no more valid. This would happen when upgrading to Dune 2.8 from earlier
     version without and building uncleaned projects. We delete these leftover
     files here. *)
  let merlin_file = ".merlin" in
  let source_dir = Path.Build.drop_build_context_exn dir in
  let merlin_in_src = Path.Source.(relative source_dir merlin_file) in
  let source_files_to_ignore =
    if
      To_delete.mem merlin_in_src
      && not (Path.Source.Set.mem source_files_to_ignore merlin_in_src)
    then (
      Log.info
        [ Pp.textf "Deleting left-over Merlin file %s.\n"
            (Path.Source.to_string merlin_in_src)
        ];
      (* We remove the file from the promoted database *)
      To_delete.remove merlin_in_src;
      Path.Source.unlink_no_err merlin_in_src;
      (* We need to keep ignoring the .merlin file for that build or Dune will
         attempt to copy it and fail because it has been deleted *)
      Path.Source.Set.add source_files_to_ignore merlin_in_src)
    else source_files_to_ignore
  in
  source_files_to_ignore

let promote_target_if_not_up_to_date ~src ~src_digest ~dst ~promote_source
    ~promote_until_clean =
  let open Fiber.O in
  (* It is OK to use [Fs_cache.Untracked.file_digest] here because below we use
     the tracked [Fs_memo.file_digest] to subscribe to the promotion result. *)
  let* promoted =
    match
      Fs_cache.read Fs_cache.Untracked.file_digest (In_source_dir dst)
      |> Cached_digest.Digest_result.to_option
    with
    | Some dst_digest when Digest.equal src_digest dst_digest ->
      (* CR-someday amokhov: We skip promotion if [src_digest = dst_digest] but
         this only works if [src] has no artifact substitution placeholders.
         Artifact substitution changes [dst]'s contents and digest, which makes
         this equality check ineffective. As a result, Dune currently executes
         [promote_source] on all such targets on every start-up. We should fix
         this, perhaps, by making artifact substitution a field of [promote]. *)
      Fiber.return false
    | _ ->
      Log.info
        [ Pp.textf "Promoting %S to %S" (Path.Build.to_string src)
            (Path.Source.to_string dst)
        ];
      if promote_until_clean then To_delete.add dst;
      (* The file in the build directory might be read-only if it comes from the
         shared cache. However, we want the file in the source tree to be
         writable by the user, so we explicitly set the user writable bit. *)
      let chmod = Path.Permissions.add Path.Permissions.write in
      let+ () =
        promote_source ~chmod ~delete_dst_if_it_is_a_directory:true ~src ~dst
      in
      true
  in
  let+ dst_digest_result =
    Memo.run (Fs_memo.file_digest ~force_update:promoted (In_source_dir dst))
  in
  match Cached_digest.Digest_result.to_option dst_digest_result with
  | Some dst_digest ->
    (* It's tempting to assert [src_digest = dst_digest] here but it turns out
       this is not necessarily true: artifact substitution can change the
       contents of promoted files. *)
    ignore dst_digest
  | None ->
    Code_error.raise
      (sprintf "Could not compute digest of promoted file %S"
         (Path.Source.to_string dst))
      [ ( "dst_digest_result"
        , Cached_digest.Digest_result.to_dyn dst_digest_result )
      ]

(* CR-someday amokhov: If a build causes N file promotions, Dune can potentially
   restart N times, which is pretty bad. It doesn't seem possible to avoid this
   with our current infrastructure. One way to improve the situation is to batch
   file promotions. Another approach is to make restarts really cheap, so that
   we don't care any more, for example, by introducing reverse dependencies in
   Memo (and/or by having smarter cancellations). *)
let promote ~dir ~(targets : _ Targets.Produced.t) ~promote ~promote_source =
  (* Select targets taking into account the (promote (only <glob>)) field. *)
  let selected_for_promotion : Path.Build.t -> bool =
    match promote.Rule.Promote.only with
    | None -> fun (_ : Path.Build.t) -> true
    | Some pred ->
      fun target ->
        Predicate.test pred
          (Path.reach ~from:(Path.build dir) (Path.build target))
  in
  let open Fiber.O in
  (* Map target paths taking into account the (promote (into <dir>)) field. *)
  let* (relocate : Path.Build.t -> Path.Source.t) =
    match promote.into with
    | None -> Fiber.return Path.Build.drop_build_context_exn
    | Some { loc; dir = into_dir } -> (
      let into_dir =
        Path.Source.relative
          (Path.Build.drop_build_context_exn dir)
          into_dir ~error_loc:loc
      in
      let promote_into_error ?unix_error msg =
        let extra_messages =
          match unix_error with
          | None -> []
          | Some error -> [ Unix_error.Detailed.pp ~prefix:"Reason: " error ]
        in
        User_error.raise ~loc
          (msg (Path.Source.to_string into_dir) :: extra_messages)
          ~annots:
            (User_message.Annots.singleton User_message.Annots.needs_stack_trace
               ())
      in
      Memo.run (Fs_memo.path_kind (In_source_dir into_dir)) >>| function
      | Ok S_DIR ->
        fun src -> Path.Source.relative into_dir (Path.Build.basename src)
      | Ok _other_kind -> promote_into_error (Pp.textf "%S is not a directory.")
      | Error (ENOENT, _, _) ->
        promote_into_error
          (Pp.textf "Directory %S does not exist. Please create it manually.")
      | Error unix_error ->
        promote_into_error ~unix_error
          (Pp.textf "Cannot promote to directory %S."))
  in
  let directory_target_error ?unix_error ~dst_dir msgs =
    let msgs =
      match unix_error with
      | None -> msgs
      | Some error -> Unix_error.Detailed.pp ~prefix:"Reason: " error :: msgs
    in
    User_error.raise
      (Pp.textf "Cannot promote files to %S." (Path.to_string dst_dir) :: msgs)
      ~annots:
        (User_message.Annots.singleton User_message.Annots.needs_stack_trace ())
  in
  let create_directory_if_needed ~dir =
    let dst_dir = relocate dir in
    (* It is OK to use [Untracked.path_stat] on [dst_dir] here because below we
       will use [Fs_memo.dir_contents] to subscribe to [dst_dir]'s contents, so
       Dune will notice its deletion. Furthermore, if we used a tracked version,
       [Path.mkdir_p] below would generate an unnecessary file-system event. *)
    match Fs_cache.(read Untracked.path_stat) (In_source_dir dst_dir) with
    | Ok { st_kind; _ } when st_kind = S_DIR -> ()
    | Error (ENOENT, _, _) -> Path.mkdir_p (Path.source dst_dir)
    | Ok _ | Error _ -> (
      (* Try to delete any unexpected stuff out of the way. In future, we might
         want to make this aggressive cleaning behaviour conditional. *)
      let dst_dir = Path.source dst_dir in
      match
        Unix_error.Detailed.catch
          (fun () ->
            Path.unlink_no_err dst_dir;
            Path.mkdir_p dst_dir)
          ()
      with
      | Ok () -> ()
      | Error unix_error -> directory_target_error ~unix_error ~dst_dir [])
  in
  (* Here we know that the promotion directory exists but we may need to create
     additional subdirectories for [targets.dirs]. *)
  Path.Build.Map.iteri targets.dirs ~f:(fun dir (_filenames : _ String.Map.t) ->
      create_directory_if_needed ~dir);
  let promote_until_clean =
    match promote.lifetime with
    | Until_clean -> true
    | Unlimited -> false
  in
  let* () =
    Fiber.sequential_iter_seq (Targets.Produced.all_files_seq targets)
      ~f:(fun (src, src_digest) ->
        match selected_for_promotion src with
        | false -> Fiber.return ()
        | true ->
          let dst = relocate src in
          promote_target_if_not_up_to_date ~src ~src_digest ~dst ~promote_source
            ~promote_until_clean)
  in
  (* There can be some files or directories left over from earlier builds, so we
     need to remove them from [targets.dirs]. *)
  let remove_stale_files_and_subdirectories ~dir ~expected_filenames =
    let dst_dir = relocate dir in
    (* We use a tracked version to subscribe to the correct directory listing.
       In this way, if a user manually creates a file inside a directory target,
       this function will rerun and remove it. *)
    Memo.run (Fs_memo.dir_contents ~force_update:true (In_source_dir dst_dir))
    >>|
    let dst_dir = Path.source dst_dir in
    function
    | Error unix_error -> directory_target_error ~unix_error ~dst_dir []
    | Ok dir_contents ->
      Fs_cache.Dir_contents.iter dir_contents ~f:(function
        | filename, S_REG ->
          if not (String.Map.mem expected_filenames filename) then
            Path.unlink_no_err (Path.relative dst_dir filename)
        | dirname, S_DIR ->
          let src_dir = Path.Build.relative dir dirname in
          if not (Path.Build.Map.mem targets.dirs src_dir) then
            Path.rm_rf (Path.relative dst_dir dirname)
        | name, _kind -> Path.unlink_no_err (Path.relative dst_dir name))
  in
  Fiber.sequential_iter_seq (Path.Build.Map.to_seq targets.dirs)
    ~f:(fun (dir, filenames) ->
      remove_stale_files_and_subdirectories ~dir ~expected_filenames:filenames)
