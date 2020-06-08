let tmp_files = ref Path.Set.empty

let () =
  at_exit (fun () ->
      let fns = !tmp_files in
      tmp_files := Path.Set.empty;
      Path.Set.iter fns ~f:Path.unlink_no_err)

let create ~prefix ~suffix =
  let fn = Path.of_string (Filename.temp_file prefix suffix) in
  tmp_files := Path.Set.add !tmp_files fn;
  fn

let destroy fn =
  Path.unlink_no_err fn;
  tmp_files := Path.Set.remove !tmp_files fn

let prng = lazy (Random.State.make_self_init ())

let temp_file_name temp_dir prefix suffix =
  let rnd = Random.State.bits (Lazy.force prng) land 0xFFFFFF in
  Filename.concat temp_dir (Printf.sprintf "%s%06x%s" prefix rnd suffix)

let dir ~prefix ~suffix =
  let dir = Filename.get_temp_dir_name () in
  let t = temp_file_name dir prefix suffix in
  let path = Path.External.of_string t in
  Path.External.mkdir_p path;
  let path = Path.external_ path in
  at_exit (fun () -> Path.rm_rf ~allow_external:true path);
  path
