let is_root t = Filename.dirname t = t

type mkdir_p =
  | Already_exists
  | Created

let rec mkdir_p ?(perms = 0o777) t_s =
  if is_root t_s then
    Already_exists
  else
    try
      Unix.mkdir t_s perms;
      Created
    with
    | Unix.Unix_error (EEXIST, _, _) -> Already_exists
    | Unix.Unix_error (ENOENT, _, _) as e ->
      let parent = Filename.dirname t_s in
      if is_root parent then
        raise e
      else
        let (_ : mkdir_p) = mkdir_p parent ~perms in
        Unix.mkdir t_s perms;
        Created
