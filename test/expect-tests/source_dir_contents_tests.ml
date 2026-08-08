open Stdune
module Dir_contents = Source.Dir_contents

let () =
  Dune_tests_common.init ();
  ignore (Dune_engine.Fs_memo.init ~dune_file_watcher:None : Memo.Invalidation.t)
;;

let run memo =
  Memo.reset Memo.Invalidation.empty;
  Fiber.run (Memo.run memo) ~iter:(fun () -> failwith "deadlock")
;;

let print_filenames label filenames =
  let filenames =
    Filename.Array.Set.to_list filenames |> List.map ~f:Filename.to_string
  in
  Printf.printf "%s: %s\n" label (String.concat filenames ~sep:", ")
;;

let%expect_test "mixed directory entries" =
  Temp.with_temp_dir
    ~parent_dir:Path.root
    ~prefix:"source-dir-contents"
    ~suffix:""
    ~f:(fun dir ->
      let dir = Result.ok_exn dir in
      Io.write_file (Path.relative dir "a.ml") "";
      Io.write_file (Path.relative dir ".#lock") "";
      Io.write_file (Path.relative dir "ignored.swp") "";
      Io.write_file (Path.relative dir "ignored~") "";
      Path.mkdir_p (Path.relative dir "dir");
      Unix.symlink "a.ml" (Path.to_string (Path.relative dir "file-link"));
      Unix.symlink "dir" (Path.to_string (Path.relative dir "dir-link"));
      Unix.symlink "missing" (Path.to_string (Path.relative dir "broken-link"));
      let path = Path.as_in_source_tree_exn dir in
      let contents =
        match run (Dir_contents.of_source_path path) with
        | Ok contents -> contents
        | Error _ -> failwith "unable to scan test directory"
      in
      print_filenames "files" (Dir_contents.files contents);
      print_filenames "dirs" (Filename.Array.Map.keys (Dir_contents.dirs contents)));
  [%expect
    {|
    files: a.ml, broken-link, file-link
    dirs: dir, dir-link |}]
;;
