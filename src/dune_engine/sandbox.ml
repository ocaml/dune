open Import

let sandbox_dir = Path.Build.relative Path.Build.root ".sandbox"

let maybe_async f =
  (* It would be nice to do this check only once and return a function, but the
     type of this function would need to be polymorphic which is forbidden by the
     relaxed value restriction. *)
  match Config.(get background_sandboxes) with
  | `Disabled ->
    let start = Time.now () in
    f ();
    let finish = Time.now () in
    Fiber.return (start, finish, None)
  | `Enabled ->
    let queue_start = Time.now () in
    let open Fiber.O in
    let+ start, finish =
      Scheduler.async_exn (fun () ->
        let start = Time.now () in
        f ();
        let finish = Time.now () in
        start, finish)
    in
    start, finish, Some (Time.diff start queue_start)
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
type snapshot = [ `Dir | `File of Cached_digest.Reduced_stats.t ] Path.Map.t

type t =
  { dir : Path.Build.t
  ; snapshot : snapshot option
  ; loc : Loc.t
  }

let dir t = t.dir
let map_path t p = Path.Build.append t.dir p

let copy_recursively =
  let chmod_file = Path.Permissions.add Path.Permissions.write in
  let chmod_dir p =
    Path.Permissions.add Path.Permissions.execute p
    |> Path.Permissions.add Path.Permissions.write
  in
  let raise_other_kind ~src kind =
    User_error.raise
      ~hints:
        [ Pp.text "Re-run Dune to delete the stale artifact, or manually delete this file"
        ]
      [ Pp.textf
          "Failed to copy file %s of kind %S while creating a copy sandbox"
          (Path.to_string_maybe_quoted src)
          (File_kind.to_string_hum kind)
      ]
  in
  let copy_file ~src ~dst = Io.copy_file ~chmod:chmod_file ~src ~dst () in
  let mkdir_with_perms ~src ~dst =
    let perms = (Unix.stat (Path.to_string src)).st_perm |> chmod_dir in
    Path.mkdir_p ~perms dst
  in
  fun ~src ~dst ->
    let { Unix.st_kind; st_perm; _ } = Unix.stat (Path.to_string src) in
    match st_kind with
    | S_REG -> copy_file ~src ~dst
    | S_DIR ->
      Path.mkdir_p ~perms:(chmod_dir st_perm) dst;
      Fpath.traverse
        ~dir:(Path.to_string src)
        ~init:()
        ~on_file:(fun ~dir fname () ->
          let rel = Filename.concat dir fname in
          let src = Path.relative src rel in
          let dst = Path.relative dst rel in
          copy_file ~src ~dst)
        ~on_dir:(fun ~dir fname () ->
          let rel = Filename.concat dir fname in
          let src = Path.relative src rel in
          let dst = Path.relative dst rel in
          mkdir_with_perms ~src ~dst)
        ~on_other:
          (`Call
              (fun ~dir fname kind () ->
                let src = Path.relative src (Filename.concat dir fname) in
                raise_other_kind ~src kind))
        ()
    | kind -> raise_other_kind ~src kind
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
         "Don't have %ss on win32, but [%s] sandboxing mode was selected. To use  \
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
     | Copy -> fun src dst -> copy_recursively ~src ~dst
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
  let root = Path.build t.dir in
  let start = Time.now () in
  let snapshot =
    Fpath.traverse
      ~dir:(Path.to_string root)
      ~init:Path.Map.empty
      ~on_dir:(fun ~dir fname acc ->
        let path = Path.relative root (Filename.concat dir fname) in
        Path.Map.add_exn acc path `Dir)
      ~on_file:(fun ~dir fname acc ->
        let p = Path.relative root (Filename.concat dir fname) in
        let stats = Unix.stat (Path.to_string p) in
        Path.Map.add_exn acc p (`File (Cached_digest.Reduced_stats.of_unix_stats stats)))
      ~on_other:`Ignore
      ~on_symlink:`Ignore
      ()
  in
  let stop = Time.now () in
  Dune_trace.emit ~buffered:true Sandbox (fun () ->
    Dune_trace.Event.sandbox `Snapshot ~start ~stop ~queued:None t.loc ~dir:t.dir);
  snapshot
;;

let create ~mode ~rule_loc ~dirs ~deps ~rule_dir ~rule_digest =
  init ();
  let sandbox_dir =
    let sandbox_suffix = rule_digest |> Digest.to_string in
    Path.Build.relative sandbox_dir sandbox_suffix
  in
  let t = { dir = sandbox_dir; snapshot = None; loc = rule_loc } in
  let open Fiber.O in
  let+ start, stop, queued =
    maybe_async (fun () ->
      Path.rm_rf (Path.build sandbox_dir);
      create_dirs t ~dirs ~rule_dir;
      (* CR-someday amokhov: Note that this doesn't link dynamic dependencies, so
         targets produced dynamically will be unavailable. *)
      link_deps t ~mode ~deps)
  in
  Dune_trace.emit ~buffered:true Sandbox (fun () ->
    Dune_trace.Event.sandbox `Create ~start ~stop ~queued t.loc ~dir:t.dir);
  match mode with
  | Patch_back_source_tree -> { t with snapshot = Some (snapshot t) }
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

let register_snapshot_promotion t (targets : Targets.Validated.t) ~old_snapshot =
  let new_snapshot = snapshot t in
  (* Same as promotion: make the file writable when copying to the source
     tree. *)
  let in_source_tree p =
    Path.extract_build_context_dir_maybe_sandboxed p |> Option.value_exn |> snd
  in
  let copy_file p =
    let source_file = in_source_tree p in
    let correction_file = Path.as_in_build_dir_exn p in
    Diff_promotion.register_intermediate `Move ~source_file ~correction_file
  in
  let delete what file = in_source_tree file |> Diff_promotion.register_delete what in
  let target_root_in_sandbox = map_path t targets.root in
  Path.Map.iter2 old_snapshot new_snapshot ~f:(fun p before after ->
    if
      not
        (let dir = Path.as_in_build_dir_exn (Path.parent_exn p) in
         Path.Build.equal dir target_root_in_sandbox
         &&
         let basename = Path.basename p in
         Filename.Set.mem targets.files basename || Filename.Set.mem targets.dirs basename)
    then (
      match before, after with
      | None, None -> assert false
      | None, Some (`File _) -> copy_file p
      | Some (`File _), None -> delete `File p
      | Some `Dir, None -> delete `Directory p
      | Some `Dir, Some `Dir -> ()
      | None, Some `Dir ->
        (* We don't create empty dirs and rely on the traversal of this dir to
           create the underlying files. Mayb e we should try harder *)
        ()
      | Some (`File _), Some `Dir ->
        (* We are going to traverse the target directory here, but we should
           really treat this as a deletion *)
        ()
      | Some `Dir, Some (`File _) -> copy_file p
      | Some (`File before), Some (`File after) ->
        (match Cached_digest.Reduced_stats.compare before after with
         | Eq -> ()
         | Lt | Gt -> copy_file p)))
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
  let open Fiber.O in
  let+ start, stop, queued =
    maybe_async (fun () ->
      Option.iter t.snapshot ~f:(fun old_snapshot ->
        register_snapshot_promotion t targets ~old_snapshot);
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
          if Fpath.exists (Path.Build.to_string src_dir)
          then Unix.rename (Path.Build.to_string src_dir) (Path.Build.to_string target)))
  in
  Dune_trace.emit ~buffered:true Sandbox (fun () ->
    Dune_trace.Event.sandbox `Extract ~start ~stop ~queued t.loc ~dir:t.dir)
;;

let failed_to_delete_sandbox dir reason =
  User_error.raise
    [ Pp.textf "failed to delete sandbox in %s" (Path.Build.to_string_maybe_quoted dir)
    ; User_error.reason reason
    ]
;;

let destroy t =
  let open Fiber.O in
  let+ start, stop, queued =
    maybe_async (fun () ->
      try Path.rm_rf ~chmod:true (Path.build t.dir) with
      | Sys_error e -> failed_to_delete_sandbox t.dir (Pp.verbatim e)
      | Unix.Unix_error (error, syscall, arg) ->
        failed_to_delete_sandbox
          t.dir
          (Unix_error.Detailed.pp (Unix_error.Detailed.create error ~syscall ~arg)))
  in
  Dune_trace.emit ~buffered:true Sandbox (fun () ->
    Dune_trace.Event.sandbox `Destroy ~start ~stop ~queued t.loc ~dir:t.dir)
;;
