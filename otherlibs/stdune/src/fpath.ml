let is_root =
  if Sys.unix
  then fun x -> x = "/" || x = "."
  else
    (* CR-someday rgrinberg: can we do better on windows? *)
    fun s ->
    Filename.dirname s = s
;;

let initial_cwd = Stdlib.Sys.getcwd ()

type mkdir_result =
  | Already_exists
  | Created
  | Missing_parent_directory

let mkdir ?(perms = 0o777) t_s =
  try
    Unix.mkdir t_s perms;
    Created
  with
  | Unix.Unix_error (EEXIST, _, _) -> Already_exists
  | Unix.Unix_error (ENOENT, _, _) -> Missing_parent_directory
;;

type mkdir_p_result =
  | Already_exists
  | Created

let rec mkdir_p ?perms t_s =
  match mkdir ?perms t_s with
  | Created -> Created
  | Already_exists -> Already_exists
  | Missing_parent_directory ->
    if is_root t_s
    then
      Code_error.raise
        "Impossible happened: [Fpath.mkdir] refused to create a directory at the root, \
         allegedly because its parent was missing"
        []
    else (
      let parent = Filename.dirname t_s in
      match mkdir_p ?perms parent with
      | Created | Already_exists ->
        (* The [Already_exists] case might happen if some other process managed
           to create the parent directory concurrently. *)
        (match mkdir t_s ?perms with
         | Created -> Created
         | Already_exists -> Already_exists
         | Missing_parent_directory ->
           (* But we just successfully created the parent directory. So it was
              likely deleted right now. Let's give up *)
           Code_error.raise "failed to create parent directory" [ "t_s", Dyn.string t_s ]))
;;

let resolve_link path =
  match Unix.readlink path with
  | exception Unix.Unix_error (EINVAL, _, _) -> Ok None
  | exception Unix.Unix_error (error, syscall, arg) ->
    Error (Dune_filesystem_stubs.Unix_error.Detailed.create ~syscall ~arg error)
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
  | Unix_error of Dune_filesystem_stubs.Unix_error.Detailed.t

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
     | _ -> raise e)
;;

let unlink_exn = if Stdlib.Sys.win32 then win32_unlink else Unix.unlink

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

let rec clear_dir dir =
  match Dune_filesystem_stubs.read_directory_with_kinds dir with
  | Error (ENOENT, _, _) -> Directory_does_not_exist
  | Error (error, _, _) ->
    raise (Unix.Unix_error (error, dir, "Stdune.Path.rm_rf: read_directory_with_kinds"))
  | Ok listing ->
    List.iter listing ~f:(fun (fn, kind) ->
      let fn = Filename.concat dir fn in
      (* Note that by the time we reach this point, [fn] might have been
         deleted by a concurrent process. Both [rm_rf_dir] and [unlink_no_err]
         will tolerate such phantom paths and succeed. *)
      match kind with
      | Unix.S_DIR -> rm_rf_dir fn
      | _ -> unlink_no_err fn);
    Cleared

and rm_rf_dir path =
  match clear_dir path with
  | Directory_does_not_exist -> ()
  | Cleared ->
    (match Unix.rmdir path with
     | () -> ()
     | exception Unix.Unix_error (ENOENT, _, _) ->
       (* How can we end up here? [clear_dir] cleared the directory successfully,
          but by the time the above [Unix.rmdir] was called, another process
          deleted the directory. *)
       ())
;;

let rm_rf fn =
  match Unix.lstat fn with
  | exception Unix.Unix_error (ENOENT, _, _) -> ()
  | { Unix.st_kind = S_DIR; _ } -> rm_rf_dir fn
  | _ -> unlink_exn fn
;;

let traverse =
  let rec loop on_file on_dir root stack acc =
    match stack with
    | [] -> acc
    | dir :: dirs ->
      let dir_path = Filename.concat root dir in
      (match Dune_filesystem_stubs.read_directory_with_kinds dir_path with
       | Error e -> Dune_filesystem_stubs.Unix_error.Detailed.raise e
       | Ok entries ->
         let stack, acc =
           List.fold_left entries ~init:(dirs, acc) ~f:(fun (stack, acc) (fname, kind) ->
             match (kind : Unix.file_kind) with
             | S_DIR -> Filename.concat dir fname :: stack, on_dir ~dir fname acc
             | S_REG -> stack, on_file ~dir fname acc
             | S_LNK ->
               let path = Filename.concat dir_path fname in
               (match (Unix.stat path).st_kind with
                | exception Unix.Unix_error (Unix.ENOENT, _, _) -> stack, acc
                | S_DIR -> Filename.concat dir fname :: stack, on_dir ~dir fname acc
                | S_REG -> stack, on_file ~dir fname acc
                | _ -> stack, acc)
             | _ -> stack, acc)
         in
         loop on_file on_dir root stack acc)
  in
  fun ~dir ~init ~on_file ~on_dir -> loop on_file on_dir dir [ "" ] init
;;

let traverse_files ~dir ~init ~f =
  traverse ~dir ~init ~on_dir:(fun ~dir:_ _fname acc -> acc) ~on_file:f
;;
