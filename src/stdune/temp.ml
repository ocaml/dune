type what =
  | Dir
  | File

let try_times n ~f =
  assert (n > 0);
  let rec loop n =
    if n = 1 then
      f n
    else
      match f n with
      | exception _ -> loop (n - 1)
      | r -> r
  in
  loop n

let prng = lazy (Random.State.make_self_init ())

let temp_file_name ~temp_dir ~prefix ~suffix =
  let rnd = Random.State.bits (Lazy.force prng) land 0xFFFFFF in
  Filename.concat temp_dir (Printf.sprintf "%s%06x%s" prefix rnd suffix)

let tmp_files = ref Path.Set.empty

let tmp_dirs = ref Path.Set.empty

let create_temp_file name =
  Unix.close (Unix.openfile name [ O_WRONLY; Unix.O_CREAT; Unix.O_EXCL ] 0o600)

let destroy = function
  | Dir -> Path.rm_rf ~allow_external:true
  | File -> Path.unlink_no_err

let create_temp_dir name =
  match Fpath.mkdir_p name with
  | Created -> ()
  | Already_exists -> raise (Unix.Unix_error (ENOENT, "mkdir", name))

let set = function
  | Dir -> tmp_dirs
  | File -> tmp_files

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

let temp ~set ~prefix ~suffix ~create =
  let path =
    let temp_dir = Filename.get_temp_dir_name () in
    try_times 1000 ~f:(fun _ ->
        let name = temp_file_name ~temp_dir ~prefix ~suffix in
        create name;
        name)
    |> Path.of_string
  in
  set := Path.Set.add !set path;
  path

let create what ~prefix ~suffix =
  let create =
    match what with
    | Dir -> create_temp_dir
    | File -> create_temp_file
  in
  let set = set what in
  temp ~set ~prefix ~suffix ~create

let destroy what fn =
  destroy what fn;
  let set = set what in
  set := Path.Set.remove !set fn
