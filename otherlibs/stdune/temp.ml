type what =
  | Dir
  | File

let prng = lazy (Random.State.make_self_init ())

let try_paths n ~dir ~prefix ~suffix ~f =
  assert (n > 0);
  let prefix = if prefix = "" then "" else prefix ^ "_" in
  let suffix = if suffix = "" then "" else "_" ^ suffix in
  let rec loop n =
    let path =
      let rnd = Random.State.bits (Lazy.force prng) land 0xFFFFFF in
      Path.relative dir (Printf.sprintf "%s%06x%s" prefix rnd suffix)
    in
    match f path with
    | Ok res -> res
    | Error `Retry ->
      if n = 1 then
        Code_error.raise "[Temp.try_paths] failed to find a good candidate" []
      else loop (n - 1)
  in
  loop n

let tmp_files = ref Path.Set.empty

let tmp_dirs = ref Path.Set.empty

let create_temp_file ?(perms = 0o600) path =
  let file = Path.to_string path in
  match
    Unix.close
      (Unix.openfile file [ O_WRONLY; Unix.O_CREAT; Unix.O_EXCL ] perms)
  with
  | () -> Ok ()
  | exception Unix.Unix_error (EEXIST, _, _) -> Error `Retry

let destroy = function
  | Dir -> Path.rm_rf ~allow_external:true
  | File -> Path.unlink_no_err

let create_temp_dir ?perms path =
  let dir = Path.to_string path in
  match Fpath.mkdir ?perms dir with
  | Created -> Ok ()
  | Already_exists -> Error `Retry
  | Missing_parent_directory | Parent_not_directory ->
    Code_error.raise "[Temp.create_temp_dir] called in a non-existing directory"
      []

let set = function
  | Dir -> tmp_dirs
  | File -> tmp_files

let create ?perms = function
  | Dir -> create_temp_dir ?perms
  | File -> create_temp_file ?perms

let () =
  let iter_and_clear r ~f =
    let tmp = !r in
    r := Path.Set.empty;
    Path.Set.iter tmp ~f
  in
  at_exit (fun () ->
      List.iter [ Dir; File ] ~f:(fun what ->
          let set = set what in
          iter_and_clear set ~f:(destroy what)))

let temp_in_dir ?perms what ~dir ~prefix ~suffix =
  let path =
    let create = create ?perms what in
    try_paths 1000 ~dir ~prefix ~suffix ~f:(fun path ->
        Result.map (create path) ~f:(fun () -> path))
  in
  let set = set what in
  set := Path.Set.add !set path;
  path

let create ?perms what ~prefix ~suffix =
  let dir =
    (* CR-someday amokhov: There are two issues with this: (i) we run this code
       every time we create a temporary file, which seems unnecessary; (ii) the
       resulting [dir] may end up being on a different partition, which could be
       a problem if we are using temporary files for atomic file operations.
       Perhaps, we should use something like [_build/.temp] instead? *)
    Filename.get_temp_dir_name () |> Path.of_filename_relative_to_initial_cwd
  in
  temp_in_dir ?perms what ~dir ~prefix ~suffix

let destroy what fn =
  destroy what fn;
  let set = set what in
  set := Path.Set.remove !set fn

let clear_dir dir =
  (match Path.clear_dir dir with
  | Cleared -> ()
  | Directory_does_not_exist ->
    (* We can end up here if the temporary directory has already been cleared,
       e.g. manually by the caller of [create Dir]. *)
    ());
  let remove_from_set ~set =
    set :=
      Path.Set.filter !set ~f:(fun f ->
          let removed =
            (not (Path.equal f dir)) && Path.is_descendant ~of_:dir f
          in
          not removed)
  in
  remove_from_set ~set:tmp_files;
  remove_from_set ~set:tmp_dirs

let temp_file =
  try_paths 1000 ~f:(fun candidate ->
      Result.map (create_temp_file candidate) ~f:(fun () -> candidate))

let temp_dir ~parent_dir =
  try_paths 1000 ~dir:parent_dir ~f:(fun candidate ->
      Result.map (create_temp_dir candidate) ~f:(fun () -> candidate))

module Monad (M : sig
  type 'a t

  val protect : f:(unit -> 'a t) -> finally:(unit -> unit) -> 'a t
end) =
struct
  let with_temp_file ~dir ~prefix ~suffix ~f =
    match temp_file ~dir ~prefix ~suffix with
    | exception e -> f (Error e)
    | temp_file ->
      M.protect
        ~f:(fun () -> f (Ok temp_file))
        ~finally:(fun () -> Path.unlink_no_err temp_file)

  let with_temp_dir ~parent_dir ~prefix ~suffix ~f =
    match temp_dir ~parent_dir ~prefix ~suffix with
    | exception e -> f (Error e)
    | temp_dir ->
      M.protect
        ~f:(fun () -> f (Ok temp_dir))
        ~finally:(fun () -> Path.rm_rf ~allow_external:true temp_dir)
end

module Id = Monad (struct
  type 'a t = 'a

  let protect = Exn.protect
end)

let with_temp_file = Id.with_temp_file

let with_temp_dir = Id.with_temp_dir
