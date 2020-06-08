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

module Dir = struct
  type t = string

  let prng = lazy (Random.State.make_self_init ())

  let temp_file_name temp_dir prefix suffix =
    let rnd = Random.State.bits (Lazy.force prng) land 0xFFFFFF in
    Filename.concat temp_dir (Printf.sprintf "%s%06x%s" prefix rnd suffix)

  let create ~for_script =
    let dir = Filename.get_temp_dir_name () in
    let t = temp_file_name dir ".dune.cram." (Filename.basename for_script) in
    let path = Path.External.of_string t in
    Path.External.mkdir_p path;
    at_exit (fun () -> Path.rm_rf ~allow_external:true (Path.external_ path));
    t

  let file t ~suffix = Filename.temp_file ~temp_dir:t "" suffix

  let open_file t ~suffix =
    let fn, oc =
      Filename.open_temp_file ~temp_dir:t "" suffix ~mode:[ Open_binary ]
    in
    (fn, oc)
end
