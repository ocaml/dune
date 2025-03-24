open Import

let sandbox_dir = Path.Build.relative Path.Build.root ".sandbox"

let maybe_async f =
  (* It would be nice to do this check only once and return a function, but the
     type of this function would need to be polymorphic which is forbidden by the
     relaxed value restriction. *)
  match Config.(get background_sandboxes) with
  | `Disabled -> Fiber.return (f ())
  | `Enabled -> Scheduler.async_exn f
;;

let init =
  let init =
    lazy
      (let dir = Path.build sandbox_dir in
       Path.mkdir_p (Path.relative dir ".hg");
       (* We create an empty [.git] file to prevent git from escaping the
          sandbox. It will choke on this empty .git and report an error about
          its format being invalid. *)
       Io.write_file (Path.relative dir ".git") "";
       (* We create a [.hg/requires] file to prevent hg from escaping the
          sandbox. It will complain that "Escaping the Dune sandbox" is an
          unknown feature. *)
       Io.write_file (Path.relative dir ".hg/requires") "Escaping the Dune sandbox")
  in
  fun () -> Lazy.force init
;;

(* Snapshot used to detect modifications. We use the same algorithm as
   [Cached_digest] given that we are trying to detect the same kind of
   changes. *)
type snapshot = Cached_digest.Reduced_stats.t Path.Map.t

type t =
  { dir : Path.Build.t
  ; snapshot : snapshot option
  }

let dir t = t.dir
let map_path t p = Path.Build.append t.dir p

module Item = struct
  type t =
    | File
    | Directory of { perms : int }
    | Link
    | Other of Unix.file_kind

  let of_path path =
    let { Unix.st_kind; st_perm; _ } = Path.Untracked.stat_exn path in
    match st_kind with
    | S_DIR -> Directory { perms = st_perm }
    | S_REG -> File
    | kind -> Other kind
  ;;

  let of_kind path (kind : Unix.file_kind) =
    match kind with
    | S_DIR -> Directory { perms = (Path.Untracked.stat_exn path).st_perm }
    | S_REG -> File
    | S_LNK -> Link
    | _ -> Other kind
  ;;
end

let copy_recursively =
  let chmod_file = Path.Permissions.add Path.Permissions.write in
  let chmod_dir p =
    Path.Permissions.add Path.Permissions.execute p
    |> Path.Permissions.add Path.Permissions.write
  in
  let rec loop item ~src ~dst =
    match (item : Item.t) with
    | Link ->
      (match Path.Untracked.stat_exn src with
       | { Unix.st_kind = S_REG; _ } -> Io.copy_file ~chmod:chmod_file ~src ~dst ()
       | { Unix.st_kind = S_DIR; st_perm = perms; _ } ->
         loop (Directory { perms }) ~src ~dst
       | { Unix.st_kind; _ } -> loop (Other st_kind) ~src ~dst)
    | File -> Io.copy_file ~chmod:chmod_file ~src ~dst ()
    | Directory { perms } ->
      (match Path.Untracked.readdir_unsorted_with_kinds src with
       | Error e -> Unix_error.Detailed.raise e
       | Ok contents ->
         let perms = chmod_dir perms in
         Path.mkdir_p ~perms dst;
         List.iter contents ~f:(fun (name, kind) ->
           let src = Path.relative src name in
           let item = Item.of_kind src kind in
           loop item ~src ~dst:(Path.relative dst name)))
    | Other kind ->
      User_error.raise
        ~hints:
          [ Pp.text
              "Re-run Dune to delete the stale artifact, or manually delete this file"
          ]
        [ Pp.textf
            "Failed to copy file %s of kind %S while creating a copy sandbox"
            (Path.to_string_maybe_quoted src)
            (File_kind.to_string_hum kind)
        ]
  in
  loop
;;

let create_dir t dir = Path.mkdir_p (Path.build (map_path t dir))

let create_dirs t ~dirs ~rule_dir =
  create_dir t rule_dir;
  Path.Build.Set.iter dirs ~f:(fun dir -> create_dir t dir)
;;

let link_function ~(mode : Sandbox_mode.some) =
  let win32_error mode =
    let mode = Sandbox_mode.to_string (Some mode) in
    Code_error.raise
      (sprintf
         "Don't have %ss on win32, but [%s] sandboxing mode was selected. To use \
          emulation via copy, the [copy] sandboxing mode should be selected."
         mode
         mode)
      []
  in
  Staged.stage
    (match mode with
     | Symlink ->
       (match Sys.win32 with
        | true -> win32_error mode
        | false -> fun src dst -> Io.portable_symlink ~src ~dst)
     | Copy ->
       fun src dst ->
         let what = Item.of_path src in
         copy_recursively what ~src ~dst
     | Hardlink ->
       (match Sys.win32 with
        | true -> win32_error mode
        | false -> fun src dst -> Io.portable_hardlink ~src ~dst)
     | Patch_back_source_tree ->
       (* We need to let the action modify its dependencies, so we copy
          dependencies and make them writable. *)
       (* CR-someday: this doesn't work with directory targets *)
       let chmod = Path.Permissions.add Path.Permissions.write in
       fun src dst -> Io.copy_file ~src ~dst ~chmod ())
;;

let link_deps t ~mode ~deps =
  let link = Staged.unstage (link_function ~mode) in
  Path.Set.iter deps ~f:(fun path ->
    match Path.as_in_build_dir path with
    | None ->
      (* This can actually raise if we try to sandbox the "copy from source
         dir" rules. There is no reason to do that though. *)
      if Path.is_in_source_tree path
      then
        Code_error.raise
          "Action depends on source tree. All actions should depend on the copies in the \
           build directory instead."
          [ "path", Path.to_dyn path ]
    | Some p -> link path (Path.build (map_path t p)))
;;

let snapshot t =
  (* CR-someday jeremiedimino: we do this kind of traversal in other places.
     Might be worth trying to factorise the code. *)
  let rec walk dir acc =
    match Path.Untracked.readdir_unsorted_with_kinds dir with
    | Error (err, func, arg) -> raise (Unix.Unix_error (err, func, arg))
    | Ok files ->
      List.fold_left files ~init:acc ~f:(fun acc (basename, (kind : Unix.file_kind)) ->
        let p = Path.relative dir basename in
        match kind with
        | S_REG ->
          let stats = Path.Untracked.lstat_exn p in
          Path.Map.add_exn acc p (Cached_digest.Reduced_stats.of_unix_stats stats)
        | S_DIR -> walk p acc
        | _ -> acc)
  in
  walk (Path.build t.dir) Path.Map.empty
;;

let create ~mode ~dune_stats ~rule_loc ~dirs ~deps ~rule_dir ~rule_digest =
  let event =
    Dune_stats.start dune_stats (fun () ->
      let cat = Some [ "create-sandbox" ] in
      let name = Loc.to_file_colon_line rule_loc in
      let args = None in
      { cat; name; args })
  in
  init ();
  let sandbox_dir =
    let sandbox_suffix = rule_digest |> Digest.to_string in
    Path.Build.relative sandbox_dir sandbox_suffix
  in
  let t = { dir = sandbox_dir; snapshot = None } in
  let open Fiber.O in
  let+ () =
    maybe_async (fun () ->
      Path.rm_rf (Path.build sandbox_dir);
      create_dirs t ~dirs ~rule_dir;
      (* CR-someday amokhov: Note that this doesn't link dynamic dependencies, so
         targets produced dynamically will be unavailable. *)
      link_deps t ~mode ~deps)
  in
  Dune_stats.finish event;
  match mode with
  | Patch_back_source_tree ->
    (* Only supported on Linux because we rely on the mtime changing to detect
       when a file changes. This doesn't work on OSX for instance as the file
       system granularity is 1s, which is too coarse. *)
    (match Platform.OS.value with
     | Linux -> ()
     | _ ->
       User_error.raise
         ~loc:rule_loc
         [ Pp.textf
             "(mode patch-back-source-tree) is only supported on Linux at the moment."
         ]);
    (* We expect this call to [snapshot t] to return the same set of files as
       [deps], given that's exactly what we just copied in the sandbox. So in
       theory, we could iterate over [deps] rather than scan the file system.
       However, the code is simpler if we just call [snapshot t] before and
       after running the action. Given that [patch_back_source_tree] is a dodgy
       feature that we hope to get rid of in the long run, we favor code
       simplicity over performance. *)
    { t with snapshot = Some (snapshot t) }
  | _ -> t
;;

(* Same as [rename] except that if the source doesn't exist we delete the
   destination *)
let rename_optional_file ~src ~dst =
  let src = Path.Build.to_string src in
  let dst = Path.Build.to_string dst in
  match Unix.rename src dst with
  | () -> ()
  | exception Unix.Unix_error ((ENOENT | ENOTDIR), _, _) ->
    (match Unix.unlink dst with
     | exception Unix.Unix_error (ENOENT, _, _) -> ()
     | () -> ())
;;

let apply_changes_to_source_tree t ~old_snapshot =
  let new_snapshot = snapshot t in
  (* Same as promotion: make the file writable when copying to the source
     tree. *)
  let in_source_tree p =
    Path.extract_build_context_dir_maybe_sandboxed p
    |> Option.value_exn
    |> snd
    |> Path.source
  in
  let copy_file p =
    let in_source_tree = in_source_tree p in
    Path.unlink_no_err in_source_tree;
    Option.iter (Path.parent in_source_tree) ~f:Path.mkdir_p;
    Io.copy_file ~src:p ~dst:in_source_tree ()
  in
  let delete_file p =
    let in_source_tree = in_source_tree p in
    Path.unlink_no_err in_source_tree
  in
  Path.Map.iter2 old_snapshot new_snapshot ~f:(fun p before after ->
    match before, after with
    | None, None -> assert false
    | None, Some _ -> copy_file p
    | Some _, None -> delete_file p
    | Some before, Some after ->
      (match Cached_digest.Reduced_stats.compare before after with
       | Eq -> ()
       | Lt | Gt -> copy_file p))
;;

let hint_delete_dir =
  [ Pp.text
      "delete this file manually or check the permissions of the parent directory of \
       this file"
  ]
;;

let move_targets_to_build_dir t ~should_be_skipped ~(targets : Targets.Validated.t)
  : unit Fiber.t
  =
  maybe_async (fun () ->
    Option.iter t.snapshot ~f:(fun old_snapshot ->
      apply_changes_to_source_tree t ~old_snapshot);
    Targets.Validated.iter
      targets
      ~file:(fun target ->
        if not (should_be_skipped target)
        then rename_optional_file ~src:(map_path t target) ~dst:target)
      ~dir:(fun target ->
        let src_dir = map_path t target in
        (match Path.Untracked.stat (Path.build target) with
         | Error (Unix.ENOENT, _, _) -> ()
         | Error e ->
           User_error.raise
             ~hints:hint_delete_dir
             [ Pp.textf "unable to stat %s" (Path.Build.to_string_maybe_quoted target)
             ; Pp.text "reason:"
             ; Pp.text (Unix_error.Detailed.to_string_hum e)
             ]
         | Ok { Unix.st_kind; _ } ->
           (* We clean up all targets (including directory targets) before
              running an action, so this branch should be unreachable unless
              the rule somehow escaped the sandbox *)
           User_error.raise
             ~hints:hint_delete_dir
             [ Pp.textf
                 "Target %s of kind %S already exists in the build directory"
                 (Path.Build.to_string_maybe_quoted target)
                 (File_kind.to_string_hum st_kind)
             ]);
        if Path.Untracked.exists (Path.build src_dir)
        then Path.rename (Path.build src_dir) (Path.build target)))
;;

let failed_to_delete_sandbox dir reason =
  User_error.raise
    [ Pp.textf "failed to delete sandbox in %s" (Path.Build.to_string_maybe_quoted dir)
    ; Pp.text "Reason:"
    ; reason
    ]
;;

let destroy t =
  maybe_async (fun () ->
    try Path.rm_rf (Path.build t.dir) with
    | Sys_error e -> failed_to_delete_sandbox t.dir (Pp.verbatim e)
    | Unix.Unix_error (error, syscall, arg) ->
      failed_to_delete_sandbox
        t.dir
        (Unix_error.Detailed.pp (Unix_error.Detailed.create error ~syscall ~arg)))
;;
