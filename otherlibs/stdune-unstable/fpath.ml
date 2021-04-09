let is_root t = Filename.dirname t = t

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

type mkdir_p_result =
  | Already_exists
  | Created

let rec mkdir_p ?(perms = 0o777) t_s =
  match mkdir ~perms t_s with
  | Created -> Created
  | Already_exists -> Already_exists
  | Missing_parent_directory -> (
    if is_root t_s then
      Code_error.raise
        "Impossible happened: [Fpath.mkdir] refused to create a directory at \
         the root, allegedly because its parent was missing"
        []
    else
      let parent = Filename.dirname t_s in
      match mkdir_p ~perms parent with
      | Created
      | Already_exists ->
        (* The [Already_exists] case might happen if some other process managed
           to create the parent directory concurrently. *)
        Unix.mkdir t_s perms;
        Created)

let resolve_link path =
  match Unix.readlink path with
  | exception Unix.Unix_error (EINVAL, _, _) -> Ok None
  | exception Unix.Unix_error (e, _, _) -> Error e
  | link ->
    Ok
      (Some
         (if Filename.is_relative link then
           Filename.concat (Filename.dirname path) link
         else
           link))

type follow_symlink_error =
  | Not_a_symlink
  | Max_depth_exceeded
  | Unix_error of Unix.error

let follow_symlink path =
  let rec loop n path =
    if n = 0 then
      Error Max_depth_exceeded
    else
      match resolve_link path with
      | Error e -> Error (Unix_error e)
      | Ok None -> Ok path
      | Ok (Some path) -> loop (n - 1) path
  in
  match resolve_link path with
  | Ok None -> Error Not_a_symlink
  | Ok (Some p) -> loop 20 p
  | Error e -> Error (Unix_error e)

let win32_unlink fn =
  try Unix.unlink fn with
  | Unix.Unix_error (Unix.EACCES, _, _) as e -> (
    try
      (* Try removing the read-only attribute *)
      Unix.chmod fn 0o666;
      Unix.unlink fn
    with
    | _ -> raise e)

let unlink =
  if Stdlib.Sys.win32 then
    win32_unlink
  else
    Unix.unlink

let unlink_no_err t =
  try unlink t with
  | _ -> ()
