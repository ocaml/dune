open! Stdune
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
        if Path.Set.mem db p then
          None
        else
          Some (Path.Set.add db p))

  let remove p =
    let p = Path.source p in
    modify_db (fun db ->
        if Path.Set.mem db p then
          Some (Path.Set.remove db p)
        else
          None)

  let dump () =
    if !needs_dumping && Path.build_dir_exists () then (
      needs_dumping := false;
      get_db () |> P.dump fn
    )

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
      Path.Source.Set.add source_files_to_ignore merlin_in_src
    ) else
      source_files_to_ignore
  in
  source_files_to_ignore

(* CR-someday amokhov: If a build causes N file promotions we will potentially
   restart it N times, which is pretty bad. It doesn't seem possible to avoid
   this with our current infrastructure. One way to improve the situation is to
   somehow batch file promotions. Another approach is to make restarts really
   cheap, so that we don't care any more, for example, by introducing reverse
   dependencies in Memo (and/or by having smarter cancellations). *)
let promote ~dir ~targets_and_digests ~promote ~promote_source =
  let selected_for_promotion =
    match promote.Rule.Promote.only with
    | None -> fun (_ : Path.Build.t) -> true
    | Some pred ->
      fun target ->
        Predicate_lang.Glob.exec pred ~standard:Predicate_lang.any
          (Path.reach ~from:(Path.build dir) (Path.build target))
  in
  let relocate =
    match promote.into with
    | None -> Fun.id
    | Some { loc; dir } ->
      fun target_in_source_tree ->
        Path.Source.relative
          (Path.Source.relative
             (Path.Source.parent_exn target_in_source_tree)
             dir ~error_loc:loc)
          (Path.Source.basename target_in_source_tree)
  in
  let open Fiber.O in
  (* CR-someday amokhov: When promoting directory targets, we might want to
     create the destination directory instead of reporting an error. Maybe we
     should just always do that? After all, if the user says "promote results
     into this directory, please", they might expect Dune to create it too. *)
  let ensure_the_destination_directory_exists ~dst_path =
    let dir = Path.parent_exn dst_path in
    Memo.Build.run (Fs_memo.path_exists dir) >>| function
    | true -> ()
    | false ->
      let loc =
        match promote.into with
        | Some into -> into.loc
        | None ->
          (* CR-someday amokhov: It's not entirely clear why this is a code
             error. If the user deletes the source directory (along with the
             corresponding [dune] file), we are going to hit this branch, and
             presumably this isn't Dune's fault. *)
          Code_error.raise
            "Promoting into a directory that does not exist. Perhaps, the user \
             deleted it while Dune was running?"
            [ ("dst_path", Path.to_dyn dst_path) ]
      in
      User_error.raise ~loc
        [ Pp.textf "directory %S does not exist"
            (Path.to_string_maybe_quoted dir)
        ]
  in
  Fiber.parallel_iter targets_and_digests ~f:(fun (target, _target_digest) ->
      match selected_for_promotion target with
      | false -> Fiber.return ()
      | true -> (
        let dst = relocate (Path.Build.drop_build_context_exn target) in
        let dst_path = Path.source dst in
        let* () = ensure_the_destination_directory_exists ~dst_path in
        Log.info
          [ Pp.textf "Promoting %S to %S"
              (Path.Build.to_string target)
              (Path.to_string dst_path)
          ];
        (match promote.lifetime with
        | Until_clean -> To_delete.add dst
        | Unlimited -> ());
        (* The file in the build directory might be read-only if it comes from
           the shared cache. However, we want the file in the source tree to be
           writable by the user, so we explicitly set the user writable bit. *)
        let chmod n = n lor 0o200 in
        let* () = promote_source ~chmod ~src:target ~dst in
        let+ dst_digest_result =
          Memo.Build.run (Fs_memo.path_digest ~force_update:true dst_path)
        in
        match Cached_digest.Digest_result.to_option dst_digest_result with
        | Some dst_digest ->
          (* It's tempting to assert [target_digest = dst_digest] here but it
             turns out this is not necessarily true: artifact substitution can
             change the contents of promoted files. *)
          ignore dst_digest
        | None ->
          Code_error.raise
            (sprintf "Counld not compute digest of promoted file %S"
               (Path.to_string dst_path))
            [ ( "dst_digest_result"
              , Cached_digest.Digest_result.to_dyn dst_digest_result )
            ]))
