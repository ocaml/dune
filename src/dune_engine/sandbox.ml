open! Stdune
open Import

let sandbox_dir = Path.Build.relative Path.Build.root ".sandbox"

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
          unkown feature. *)
       Io.write_file
         (Path.relative dir ".hg/requires")
         "Escaping the Dune sandbox")
  in
  fun () -> Lazy.force init

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

let create_dirs t ~deps ~chdirs ~rule_dir =
  Path.Set.iter
    (Path.Set.add
       (Path.Set.union_all
          [ chdirs; Dep.Facts.parent_dirs deps; Dep.Facts.dirs deps ])
       (Path.build rule_dir))
    ~f:(fun path ->
      match Path.as_in_build_dir path with
      | None ->
        (* This [path] is not in the build directory, so we do not need to
           create it. If it comes from [deps], it must exist already. If it
           comes from [chdirs], we'll ensure that it exists in the call to
           [Fs.mkdir_p_or_assert_existence] below. *)
        ()
      | Some path ->
        (* There is no point in using the memoized version [Fs.mkdir_p] since
           these directories are not shared between actions. *)
        Path.mkdir_p (Path.build (map_path t path)))

let link_function ~(mode : Sandbox_mode.some) =
  let win32_error mode =
    let mode = Sandbox_mode.to_string (Some mode) in
    Code_error.raise
      (sprintf
         "Don't have %ss on win32, but [%s] sandboxing mode was selected. To \
          use emulation via copy, the [copy] sandboxing mode should be \
          selected."
         mode mode)
      []
  in
  Staged.stage
    (match mode with
    | Symlink -> (
      match Sys.win32 with
      | true -> win32_error mode
      | false -> fun src dst -> Io.portable_symlink ~src ~dst)
    | Copy -> fun src dst -> Io.copy_file ~src ~dst ()
    | Hardlink -> (
      match Sys.win32 with
      | true -> win32_error mode
      | false -> fun src dst -> Io.portable_hardlink ~src ~dst))

let copy_and_make_writable src dst =
  Io.copy_file ~src ~dst ~chmod:(fun n -> n lor 0o200) ()

let link_deps t ~mode ~patch_back_source_tree ~deps =
  let link =
    if patch_back_source_tree then
      (* We need to let the action modify its dependencies, so we copy
         dependencies and make them writable. *)
      copy_and_make_writable
    else
      Staged.unstage (link_function ~mode)
  in
  Path.Map.iteri deps ~f:(fun path (_ : Digest.t) ->
      match Path.as_in_build_dir path with
      | None ->
        (* This can actually raise if we try to sandbox the "copy from source
           dir" rules. There is no reason to do that though. *)
        if Path.is_in_source_tree path then
          Code_error.raise
            "Action depends on source tree. All actions should depend on the \
             copies in the build directory instead."
            [ ("path", Path.to_dyn path) ]
      | Some p -> link path (Path.build (map_path t p)))

let snapshot t =
  (* CR-someday jeremiedimino: we do this kind of traversal in other places.
     Might be worth trying to factorise the code. *)
  let rec walk dir acc =
    match Path.Untracked.readdir_unsorted dir with
    | Error (err, func, arg) -> raise (Unix.Unix_error (err, func, arg))
    | Ok files ->
      List.fold_left files ~init:acc ~f:(fun acc basename ->
          let p = Path.relative dir basename in
          let stats = Path.Untracked.lstat_exn p in
          match stats.st_kind with
          | S_REG ->
            Path.Map.add_exn acc p
              (Cached_digest.Reduced_stats.of_unix_stats stats)
          | S_DIR -> walk p acc
          | _ -> acc)
  in
  walk (Path.build t.dir) Path.Map.empty

let create ~mode ~patch_back_source_tree ~rule_loc ~deps ~rule_dir ~chdirs
    ~rule_digest ~expand_aliases =
  init ();
  let sandbox_suffix = rule_digest |> Digest.to_string in
  let sandbox_dir = Path.Build.relative sandbox_dir sandbox_suffix in
  let t = { dir = sandbox_dir; snapshot = None } in
  Path.rm_rf (Path.build sandbox_dir);
  create_dirs t ~deps ~chdirs ~rule_dir;
  let deps =
    if expand_aliases then
      Dep.Facts.paths deps
    else
      Dep.Facts.paths_without_expanding_aliases deps
  in
  (* CR-someday amokhov: Note that this doesn't link dynamic dependencies, so
     targets produced dynamically will be unavailable. *)
  link_deps t ~mode ~patch_back_source_tree ~deps;
  if patch_back_source_tree then (
    (* Only supported on Linux because we rely on the mtime changing to detect
       when a file changes. This doesn't work on OSX for instance as the file
       system granularity is 1s, which is too coarse. *)
    if not Sys.linux then
      User_error.raise ~loc:rule_loc
        [ Pp.textf
            "(mode patch-back-source-tree) is only supported on Linux at the \
             moment."
        ];
    (* We expect this call to [snapshot t] to return the same set of files as
       [deps], given that's exactly what we just copied in the sandbox. So in
       theory, we could iterate over [deps] rather than scan the file system.
       However, the code is simpler if we just call [snapshot t] before and
       after running the action. Given that [patch_back_source_tree] is a dodgy
       feature that we hope to get rid of in the long run, we favor code
       simplicity over performance. *)
    { t with snapshot = Some (snapshot t) }
  ) else
    t

(* Same as [rename] except that if the source doesn't exist we delete the
   destination *)
let rename_optional_file ~src ~dst =
  let src = Path.Build.to_string src in
  let dst = Path.Build.to_string dst in
  match Unix.rename src dst with
  | () -> ()
  | exception Unix.Unix_error ((ENOENT | ENOTDIR), _, _) -> (
    match Unix.unlink dst with
    | exception Unix.Unix_error (ENOENT, _, _) -> ()
    | () -> ())

(* Recursively move regular files from [src] to [dst] and return the set of
   moved files. *)
let rename_dir_recursively ~loc ~src_dir ~dst_dir =
  let rec loop ~src_dir ~dst_dir =
    (match Fpath.mkdir (Path.Build.to_string dst_dir) with
    | Created -> ()
    | Already_exists ->
      User_error.raise ~loc
        ~annots:
          (User_message.Annots.singleton User_message.Annots.needs_stack_trace
             ())
        [ Pp.textf
            "This rule defines a directory target %S whose name conflicts with \
             an internal directory used by Dune. Please use a different name."
            (Path.Build.drop_build_context_exn dst_dir
            |> Path.Source.to_string_maybe_quoted)
        ]
    | Missing_parent_directory -> assert false);
    match
      Dune_filesystem_stubs.read_directory_with_kinds
        (Path.Build.to_string src_dir)
    with
    | Ok files ->
      List.concat_map files ~f:(fun (file, kind) ->
          match (kind : Dune_filesystem_stubs.File_kind.t) with
          | S_REG ->
            let src = Path.Build.relative src_dir file in
            let dst = Path.Build.relative dst_dir file in
            Unix.rename (Path.Build.to_string src) (Path.Build.to_string dst);
            [ dst ]
          | S_DIR ->
            loop
              ~src_dir:(Path.Build.relative src_dir file)
              ~dst_dir:(Path.Build.relative dst_dir file)
          | _ ->
            User_error.raise ~loc
              [ Pp.textf "Rule produced a file with unrecognised kind %S"
                  (Dune_filesystem_stubs.File_kind.to_string kind)
              ])
    | Error (ENOENT, _, _) ->
      User_error.raise ~loc
        [ Pp.textf "Rule failed to produce directory %S"
            (Path.Build.drop_build_context_maybe_sandboxed_exn src_dir
            |> Path.Source.to_string_maybe_quoted)
        ]
    | Error (unix_error, _, _) ->
      User_error.raise ~loc
        [ Pp.textf "Rule produced unreadable directory %S"
            (Path.Build.drop_build_context_maybe_sandboxed_exn src_dir
            |> Path.Source.to_string_maybe_quoted)
        ; Pp.verbatim (Unix.error_message unix_error)
        ]
  in
  loop ~src_dir ~dst_dir |> Path.Build.Set.of_list

let apply_changes_to_source_tree t ~old_snapshot =
  let new_snapshot = snapshot t in
  (* Same as promotion: make the file writable when copying to the source
     tree. *)
  let in_source_tree p =
    Path.extract_build_context_dir_maybe_sandboxed p
    |> Option.value_exn |> snd |> Path.source
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
      match (before, after) with
      | None, None -> assert false
      | None, Some _ -> copy_file p
      | Some _, None -> delete_file p
      | Some before, Some after -> (
        match Cached_digest.Reduced_stats.compare before after with
        | Eq -> ()
        | Lt
        | Gt ->
          copy_file p))

let move_targets_to_build_dir t ~loc ~targets =
  Option.iter t.snapshot ~f:(fun old_snapshot ->
      apply_changes_to_source_tree t ~old_snapshot);
  let (_file_targets_renamed : unit list), files_moved_in_directory_targets =
    Targets.partition_map targets
      ~file:(fun target ->
        rename_optional_file ~src:(map_path t target) ~dst:target)
      ~dir:(fun target ->
        rename_dir_recursively ~loc ~src_dir:(map_path t target) ~dst_dir:target)
  in
  Path.Build.Set.union_all files_moved_in_directory_targets

let destroy t = Path.rm_rf (Path.build t.dir)
