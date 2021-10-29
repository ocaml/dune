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

let promote ~dir ~targets ~promote ~promote_source =
  let open Fiber.O in
  let { Rule.Promote.lifetime; only; into; _ } = promote in
  (* CR-someday amokhov: Don't ignore directory targets. *)
  let file_targets =
    Targets.map targets ~f:(fun ~files ~dirs ->
        ignore dirs;
        files)
  in
  Fiber.parallel_iter_set
    (module Path.Build.Set)
    file_targets
    ~f:(fun target ->
      let consider_for_promotion =
        match only with
        | None -> true
        | Some pred ->
          Predicate_lang.Glob.exec pred
            (Path.reach (Path.build target) ~from:(Path.build dir))
            ~standard:Predicate_lang.any
      in
      match consider_for_promotion with
      | false -> Fiber.return ()
      | true ->
        let in_source_tree = Path.Build.drop_build_context_exn target in
        let in_source_tree =
          match into with
          | None -> in_source_tree
          | Some { loc; dir } ->
            Path.Source.relative
              (Path.Source.relative
                 (Path.Source.parent_exn in_source_tree)
                 dir ~error_loc:loc)
              (Path.Source.basename in_source_tree)
        in
        let* () =
          let dir = Path.Source.parent_exn in_source_tree in
          Memo.Build.run (Source_tree.find_dir dir) >>| function
          | Some _ -> ()
          | None ->
            let loc =
              match into with
              | Some into -> into.loc
              | None ->
                Code_error.raise "promoting into directory that does not exist"
                  [ ("in_source_tree", Path.Source.to_dyn in_source_tree) ]
            in
            User_error.raise ~loc
              [ Pp.textf "directory %S does not exist"
                  (Path.Source.to_string_maybe_quoted dir)
              ]
        in
        let dst = in_source_tree in
        let in_source_tree = Path.source in_source_tree in
        let* is_up_to_date =
          Memo.Build.run
            (let open Memo.Build.O in
            Fs_memo.path_digest in_source_tree
            >>| Cached_digest.Digest_result.to_option
            >>| function
            | None -> false
            | Some in_source_tree_digest -> (
              match
                Cached_digest.build_file target
                |> Cached_digest.Digest_result.to_option
              with
              | None ->
                (* CR-someday amokhov: We couldn't digest the target so
                   something happened to it. Right now, we skip the promotion in
                   this case, but we could perhaps delete the corresponding path
                   in the source tree. *)
                true
              | Some in_build_dir_digest ->
                Digest.equal in_build_dir_digest in_source_tree_digest))
        in
        if is_up_to_date then
          Fiber.return ()
        else (
          if lifetime = Until_clean then To_delete.add dst;
          let* () = Scheduler.ignore_for_watch in_source_tree in
          (* The file in the build directory might be read-only if it comes from
             the shared cache. However, we want the file in the source tree to
             be writable by the user, so we explicitly set the user writable
             bit. *)
          let chmod n = n lor 0o200 in
          Path.Source.unlink_no_err dst;
          promote_source ~chmod ~src:target ~dst
        ))
