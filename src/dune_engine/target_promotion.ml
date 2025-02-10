open! Import

(* [To_delete] is used mostly to implement [dune clean]. It is an imperfect
   heuristic, in particular it can go wrong if:

   - the user deletes .to-delete-in-source-tree file

   - the user edits a previously promoted file with the intention of keeping it
     in the source tree, or creates a new file with the same name. *)
module To_delete = struct
  module P = Dune_util.Persistent.Make (struct
      type t = Path.Source.Set.t

      let name = "PROMOTED-TO-DELETE"
      let version = 2
      let to_dyn = Path.Source.Set.to_dyn
      let test_example () = Path.Source.Set.empty
    end)

  let fn = Path.relative Path.build_dir ".to-delete-in-source-tree"

  (* [db] is used to accumulate promoted files from rules. *)
  let db = lazy (ref (Option.value ~default:Path.Source.Set.empty (P.load fn)))
  let get_db () = !(Lazy.force db)
  let set_db new_db = Lazy.force db := new_db
  let needs_dumping = ref false

  let modify_db f =
    match f (get_db ()) with
    | None -> ()
    | Some new_db ->
      set_db new_db;
      needs_dumping := true
  ;;

  let add p =
    modify_db (fun db ->
      if Path.Source.Set.mem db p then None else Some (Path.Source.Set.add db p))
  ;;

  let dump () =
    if !needs_dumping && Path.build_dir_exists ()
    then (
      needs_dumping := false;
      get_db () |> P.dump fn)
  ;;

  let () = Hooks.End_of_build.always dump
end

let files_in_source_tree_to_delete () = To_delete.get_db ()

let promote_target_if_not_up_to_date
      ~src
      ~src_digest
      ~dst
      ~promote_source
      ~promote_until_clean
  =
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
        [ Pp.textf
            "Promoting %S to %S"
            (Path.Build.to_string src)
            (Path.Source.to_string dst)
        ];
      if promote_until_clean then To_delete.add dst;
      (* The file in the build directory might be read-only if it comes from the
         shared cache. However, we want the file in the source tree to be
         writable by the user, so we explicitly set the user writable bit. *)
      let chmod = Path.Permissions.add Path.Permissions.write in
      let+ () = promote_source ~chmod ~delete_dst_if_it_is_a_directory:true ~src ~dst in
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
      (sprintf "Could not compute digest of promoted file %S" (Path.Source.to_string dst))
      [ "dst_digest_result", Cached_digest.Digest_result.to_dyn dst_digest_result ]
;;

(* CR-someday amokhov: If a build causes N file promotions, Dune can potentially
   restart N times, which is pretty bad. It doesn't seem possible to avoid this
   with our current infrastructure. One way to improve the situation is to batch
   file promotions. Another approach is to make restarts really cheap, so that
   we don't care any more, for example, by introducing reverse dependencies in
   Memo (and/or by having smarter cancellations). *)
let promote ~(targets : _ Targets.Produced.t) ~(promote : Rule.Promote.t) ~promote_source =
  (* Select targets taking into account the (promote (only <glob>)) field. *)
  let selected_for_promotion : Path.Local.t -> bool =
    match promote.only with
    | None -> fun (_ : Path.Local.t) -> true
    | Some pred -> fun target -> Predicate.test pred (Path.Local.to_string target)
  in
  let open Fiber.O in
  (* Map target paths taking into account the (promote (into <dir>)) field. *)
  let* (relocate : Path.Build.t -> Path.Source.t) =
    match promote.into with
    | None -> Fiber.return Path.Build.drop_build_context_exn
    | Some { loc; dir = into_dir } ->
      let into_dir =
        Path.Source.relative
          (Path.Build.drop_build_context_exn targets.root)
          into_dir
          ~error_loc:loc
      in
      let promote_into_error ?unix_error msg =
        let extra_messages =
          match unix_error with
          | None -> []
          | Some error -> [ Unix_error.Detailed.pp ~prefix:"Reason: " error ]
        in
        User_error.raise
          ~loc
          (msg (Path.Source.to_string into_dir) :: extra_messages)
          ~annots:(User_message.Annots.singleton User_message.Annots.needs_stack_trace ())
      in
      Memo.run (Fs_memo.path_kind (In_source_dir into_dir))
      >>| (function
       | Ok S_DIR -> fun src -> Path.Source.relative into_dir (Path.Build.basename src)
       | Ok _other_kind -> promote_into_error (Pp.textf "%S is not a directory.")
       | Error (ENOENT, _, _) ->
         promote_into_error
           (Pp.textf "Directory %S does not exist. Please create it manually.")
       | Error unix_error ->
         promote_into_error ~unix_error (Pp.textf "Cannot promote to directory %S."))
  in
  let directory_target_error ?unix_error ~dst_dir msgs =
    let msgs =
      match unix_error with
      | None -> msgs
      | Some error -> Unix_error.Detailed.pp ~prefix:"Reason: " error :: msgs
    in
    User_error.raise
      (Pp.textf "Cannot promote files to %S." (Path.to_string dst_dir) :: msgs)
      ~annots:(User_message.Annots.singleton User_message.Annots.needs_stack_trace ())
  in
  let create_directory_if_needed ~dir =
    let dst_dir = relocate dir in
    (* It is OK to use [Untracked.path_stat] on [dst_dir] here because below we
       will use [Fs_memo.dir_contents] to subscribe to [dst_dir]'s contents, so
       Dune will notice its deletion. Furthermore, if we used a tracked version,
       [Path.mkdir_p] below would generate an unnecessary file-system event. *)
    match Fs_cache.(read Untracked.path_stat) (In_source_dir dst_dir) with
    | Ok { st_kind = S_DIR; _ } -> ()
    | Error (ENOENT, _, _) -> Path.mkdir_p (Path.source dst_dir)
    | Ok _ | Error _ ->
      (* Try to delete any unexpected stuff out of the way. In future, we might
         want to make this aggressive cleaning behaviour conditional. *)
      let dst_dir = Path.source dst_dir in
      (match
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
  Targets.Produced.iter_dirs targets ~f:(fun dir ->
    create_directory_if_needed ~dir:(Path.Build.append_local targets.root dir));
  let promote_until_clean =
    match promote.lifetime with
    | Until_clean -> true
    | Unlimited -> false
  in
  let* () =
    Fiber.sequential_iter_seq
      (Targets.Produced.all_files_seq targets)
      ~f:(fun (src, src_digest) ->
        match selected_for_promotion src with
        | false -> Fiber.return ()
        | true ->
          let src = Path.Build.append_local targets.root src in
          let dst = relocate src in
          promote_target_if_not_up_to_date
            ~src
            ~src_digest
            ~dst
            ~promote_source
            ~promote_until_clean)
  in
  (* There can be some files or directories left over from earlier builds, so we
     need to remove them from [targets]. *)
  let remove_stale_files_and_subdirectories ~dir =
    (* CR-someday rleshchinskiy: This can probably be made more efficient by relocating
       root once. *)
    let build_dir = Path.Build.append_local targets.root dir in
    let dst_dir = relocate build_dir in
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
        | file_name, S_REG ->
          if not (Targets.Produced.mem targets (Path.Build.relative build_dir file_name))
          then Path.unlink_no_err (Path.relative dst_dir file_name)
        | dir_name, S_DIR ->
          let src_dir = Path.Build.relative build_dir dir_name in
          if not (Targets.Produced.mem_dir targets src_dir)
          then Path.rm_rf (Path.relative dst_dir dir_name)
        | name, _kind -> Path.unlink_no_err (Path.relative dst_dir name))
  in
  Fiber.sequential_iter_seq (Targets.Produced.all_dirs_seq targets) ~f:(fun dir ->
    remove_stale_files_and_subdirectories ~dir)
;;
