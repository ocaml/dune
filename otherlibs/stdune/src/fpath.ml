let is_root =
  if Sys.unix
  then fun x -> x = "/" || x = "."
  else
    (* CR-someday rgrinberg: can we do better on windows? *)
    fun s -> Filename.dirname s = s
;;

let initial_cwd = Stdlib.Sys.getcwd ()

type mkdir_result =
  [ `Already_exists
  | `Created
  | `Missing_parent_directory
  ]

let mkdir ?(perms = 0o777) t_s =
  try
    Unix.mkdir t_s perms;
    `Created
  with
  | Unix.Unix_error (EEXIST, _, _) -> `Already_exists
  | Unix.Unix_error (ENOENT, _, _) -> `Missing_parent_directory
;;

type mkdir_p_result =
  [ `Already_exists
  | `Created
  ]

let dyn_of_mkdir_p_result = function
  | `Already_exists -> Dyn.variant "Already_exists" []
  | `Created -> Dyn.variant "Created" []
;;

let rec mkdir_p ?perms t_s =
  match mkdir ?perms t_s with
  | #mkdir_p_result as x -> x
  | `Missing_parent_directory ->
    if is_root t_s
    then
      Code_error.raise
        "Impossible happened: [Fpath.mkdir] refused to create a directory at the root, \
         allegedly because its parent was missing"
        []
    else (
      let parent = Filename.dirname t_s in
      match mkdir_p ?perms parent with
      | `Created | `Already_exists ->
        (* The [Already_exists] case might happen if some other process managed
           to create the parent directory concurrently. *)
        (match mkdir t_s ?perms with
         | #mkdir_p_result as x -> x
         | `Missing_parent_directory ->
           (* But we just successfully created the parent directory. So it was
              likely deleted right now. Let's give up *)
           Code_error.raise "failed to create parent directory" [ "t_s", Dyn.string t_s ]))
;;

let mkdir_p_strict ?perms t_s =
  match mkdir_p ?perms t_s with
  | `Created -> `Created
  | `Already_exists ->
    (match (Unix.stat t_s).st_kind with
     | S_DIR -> `Already_exists
     | _ -> `Not_a_dir)
;;

let link src dst =
  match Unix.link src dst with
  | exception Unix.Unix_error (Unix.EUNKNOWNERR -1142, syscall, arg)
  (* Needed for OCaml < 5.1 on windows *) ->
    Exn.reraise (Unix.Unix_error (Unix.EMLINK, syscall, arg))
  | s -> s
;;

let resolve_link path =
  match Unix.readlink path with
  | exception Unix.Unix_error (EINVAL, _, _) -> Ok None
  | exception Unix.Unix_error (error, syscall, arg) ->
    Error (Unix_error.Detailed.create ~syscall ~arg error)
  | link ->
    Ok
      (Some
         (if Filename.is_relative link
          then Filename.concat (Filename.dirname path) link
          else link))
;;

type follow_symlink_error =
  | Not_a_symlink
  | Max_depth_exceeded
  | Unix_error of Unix_error.Detailed.t

let follow_symlink path =
  let rec loop n path =
    if n = 0
    then Error Max_depth_exceeded
    else (
      match resolve_link path with
      | Error e -> Error (Unix_error e)
      | Ok None -> Ok path
      | Ok (Some path) -> loop (n - 1) path)
  in
  match resolve_link path with
  | Ok None -> Error Not_a_symlink
  | Ok (Some p) -> loop 20 p
  | Error e -> Error (Unix_error e)
;;

let rec follow_symlinks path =
  let parent = Filename.dirname path in
  let file = Filename.basename path in
  (* If we reached the root, just return the path. *)
  if parent = path
  then Some path
  else if parent = Filename.current_dir_name
  then
    (* Only keep the initial ["."] if it was in the path. *)
    if path = Filename.concat Filename.current_dir_name file then Some path else Some file
  else (
    (* Recurse on parent, and re-add toplevel file. *)
    match follow_symlinks parent with
    | None -> None
    | Some parent ->
      let path = Filename.concat parent file in
      (* Normalize the result. *)
      (match follow_symlink path with
       | Ok p -> Some p
       | Error Max_depth_exceeded -> None
       | Error _ -> Some path))
;;

let win32_unlink fn =
  try Unix.unlink fn with
  | Unix.Unix_error (Unix.EACCES, _, _) as e ->
    (try
       (* Try removing the read-only attribute *)
       Unix.chmod fn 0o666;
       Unix.unlink fn
     with
     | Unix.Unix_error (Unix.EACCES, _, _) ->
       (* On Windows a virus scanner frequently has a lock on new executables for a short while - just retry *)
       let rec retry_loop cnt =
         Unix.sleep 1;
         try Unix.unlink fn with
         | Unix.Unix_error (Unix.EACCES, _, _) ->
           if cnt > 0 then retry_loop (cnt - 1) else raise e
       in
       retry_loop 30)
;;

let unlink_exn = if Stdlib.Sys.win32 then win32_unlink else fun t -> Unix.unlink t

type unlink_status =
  | Success
  | Does_not_exist
  | Is_a_directory
  | Error of exn

let unlink t =
  match unlink_exn t with
  | () -> Success
  | exception exn ->
    (match exn with
     | Unix.Unix_error (ENOENT, _, _) -> Does_not_exist
     | Unix.Unix_error (error, _, _) ->
       (match error, Platform.OS.value with
        | EISDIR, _ | EPERM, Darwin -> Is_a_directory
        | _ -> Error exn)
     | _ -> Error exn)
;;

let unlink_no_err t =
  try unlink_exn t with
  | _ -> ()
;;

type clear_dir_result =
  | Cleared
  | Directory_does_not_exist

let unlink_exn_ignore_missing fn =
  match unlink_exn fn with
  | () -> ()
  | exception Unix.Unix_error (ENOENT, _, _) -> ()
;;

let rmdir_ignore_missing fn =
  match Unix.rmdir fn with
  | () -> ()
  | exception Unix.Unix_error (ENOENT, _, _) ->
    (* How can we end up here? [clear_dir] cleared the directory successfully,
       but by the time the above [Unix.rmdir] was called, another process
       deleted the directory. *)
    ()
;;

let unlink_exn ~chmod dir fn =
  match unlink_exn_ignore_missing fn with
  | () -> ()
  | exception Unix.Unix_error ((EPERM | EACCES), _, _) when chmod ->
    Unix.chmod dir 0o777;
    unlink_exn_ignore_missing fn
;;

let rec clear_dir ?(chmod = false) dir =
  match
    match Readdir.read_directory_with_kinds dir with
    | Error ((EPERM | EACCES), _, _) when chmod ->
      Unix.chmod dir 0o777;
      Readdir.read_directory_with_kinds dir
    | s -> s
  with
  | Error (ENOENT, _, _) -> Directory_does_not_exist
  | Error (error, _, _) ->
    raise (Unix.Unix_error (error, dir, "Stdune.Path.rm_rf: read_directory_with_kinds"))
  | Ok listing ->
    clear_files ~chmod dir listing;
    Cleared

and clear_files ?(chmod = false) dir listing =
  List.iter listing ~f:(fun (fn, kind) ->
    let fn = Filename.concat dir fn in
    (* Note that by the time we reach this point, [fn] might have been
       deleted by a concurrent process. Both [rm_rf_dir] and [unlink_no_err]
       will tolerate such phantom paths and succeed. *)
    match kind with
    | Unix.S_DIR -> rm_rf_dir ~chmod fn
    | _ -> unlink_exn ~chmod dir fn)

and rm_rf_dir ?(chmod = false) path =
  match clear_dir ~chmod path with
  | Directory_does_not_exist -> ()
  | Cleared ->
    (match rmdir_ignore_missing path with
     | () -> ()
     | exception Unix.Unix_error ((EPERM | EACCES), _, _) when chmod ->
       let parent = Filename.dirname path in
       Unix.chmod parent 0o777;
       rmdir_ignore_missing path)
;;

let unlink_exn ?(chmod = false) file = unlink_exn ~chmod (Filename.dirname file) file

let rm_rf ?(chmod = false) fn =
  match Unix.lstat fn with
  | exception Unix.Unix_error (ENOENT, _, _) -> ()
  | { Unix.st_kind = S_DIR; _ } -> rm_rf_dir ~chmod fn
  | _ -> unlink_exn ~chmod fn
;;

let default ~dir:_ _ acc = acc

(* CR-someday rgrinberg: maybe we should make sure that we don't hit any
   symlink loops here? *)
let traverse
      ~dir
      ~init
      ?(on_file = default)
      ?(on_dir = default)
      ?(on_other = `Raise)
      ?(on_symlink = `Resolve)
      ?(enter_dir = fun ~dir:_ _fname -> true)
      ?(on_error = `Raise)
      ()
  =
  let on_other =
    match on_other with
    | `Call f -> f
    | `Ignore -> fun ~dir:_ _fname _kind acc -> acc
    | `Raise ->
      fun ~dir fname kind _acc ->
        User_error.raise
          [ Pp.textf
              "unrecognized file kind %s in %S"
              (File_kind.to_string_hum kind)
              (Filename.concat dir fname)
          ]
  in
  let handle_kind ~dir fname kind stack acc =
    match (kind : Unix.file_kind) with
    | S_DIR ->
      (match enter_dir ~dir fname with
       | false -> stack, acc
       | true ->
         let acc = on_dir ~dir fname acc in
         let stack = Filename.concat dir fname :: stack in
         stack, acc)
    | S_REG -> stack, on_file ~dir fname acc
    | kind -> stack, on_other ~dir fname kind acc
  in
  let on_error =
    match on_error with
    | `Raise -> fun ~dir:_ err _ -> Unix_error.Detailed.raise err
    | `Ignore -> fun ~dir:_ _err acc -> acc
    | `Call f -> f
  in
  let rec loop root stack acc =
    match stack with
    | [] -> acc
    | dir :: dirs ->
      let dir_path = Filename.concat root dir in
      (match Readdir.read_directory_with_kinds dir_path with
       | Error e ->
         let acc = on_error ~dir e acc in
         loop root dirs acc
       | Ok entries ->
         let stack, acc =
           List.fold_left entries ~init:(dirs, acc) ~f:(fun (stack, acc) (fname, kind) ->
             match (kind : Unix.file_kind) with
             | S_LNK ->
               (match on_symlink with
                | `Raise ->
                  User_error.raise
                    [ Pp.textf
                        "Symlink %s is not allowed here"
                        (Filename.concat dir fname)
                    ]
                | `Ignore -> stack, acc
                | `Resolve ->
                  (match Unix.stat (Filename.concat dir_path fname) with
                   | exception Unix.Unix_error (x, y, z) ->
                     stack, on_error ~dir (x, y, z) acc
                   | stat -> handle_kind ~dir fname stat.st_kind stack acc)
                | `Call f ->
                  let acc, kind = f ~dir fname acc in
                  (match kind with
                   | None -> stack, acc
                   | Some kind -> handle_kind ~dir fname kind stack acc))
             | kind -> handle_kind ~dir fname kind stack acc)
         in
         loop root stack acc)
  in
  loop dir [ "" ] init
;;

let traverse_files ~dir ~init ~f =
  let skip = fun ~dir:_ _fname acc -> acc in
  let root = dir in
  let on_symlink ~dir fname acc =
    let path = Filename.concat (Filename.concat root dir) fname in
    match Unix.stat path with
    | { Unix.st_kind = kind; _ } -> acc, Some kind
    | exception Unix.Unix_error (Unix.ENOENT, _, _) -> acc, None
    | exception Unix.Unix_error (error, syscall, arg) ->
      Unix_error.Detailed.raise (Unix_error.Detailed.create error ~syscall ~arg)
  in
  traverse
    ~dir
    ~init
    ~on_dir:skip
    ~on_other:`Ignore
    ~on_symlink:(`Call on_symlink)
    ~on_file:f
    ()
;;

let is_broken_symlink path =
  let stats = Unix.lstat path in
  match (stats.st_kind : Unix.file_kind) with
  | S_LNK ->
    (match Unix.stat path with
     | exception Unix.Unix_error (Unix.ENOENT, _, _) -> true
     | _ -> false)
  | _ -> false
;;

let is_directory x =
  try (Unix.stat x).st_kind = S_DIR with
  | Unix.Unix_error (ENOENT, _, _) -> false
;;

let exists x =
  try
    ignore (Unix.stat x);
    true
  with
  | Unix.Unix_error (ENOENT, _, _) -> false
;;
