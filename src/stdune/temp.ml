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

external open_desc : string -> open_flag list -> int -> int = "caml_sys_open"

external close_desc : int -> unit = "caml_sys_close"

let create_temp_file name =
  close_desc (open_desc name [ Open_wronly; Open_creat; Open_excl ] 0o600)

let create_temp_dir name =
  match Fpath.mkdir_p name with
  | Created -> ()
  | Already_exists -> raise (Unix.Unix_error (ENOENT, "mkdir", name))

let () =
  let iter_and_clear r ~f =
    let tmp = !r in
    r := Path.Set.empty;
    Path.Set.iter tmp ~f
  in
  at_exit (fun () ->
      iter_and_clear tmp_files ~f:Path.unlink_no_err;
      iter_and_clear tmp_dirs ~f:(Path.rm_rf ~allow_external:true))

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

let file ~prefix ~suffix =
  temp ~set:tmp_files ~prefix ~suffix ~create:create_temp_file

let destroy_file fn =
  Path.unlink_no_err fn;
  tmp_files := Path.Set.remove !tmp_files fn

let dir ~prefix ~suffix =
  temp ~set:tmp_dirs ~prefix ~suffix ~create:create_temp_dir
