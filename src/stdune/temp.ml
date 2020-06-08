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
