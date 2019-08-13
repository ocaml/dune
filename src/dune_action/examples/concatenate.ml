open Dune_action

let concatenate_files file1 file2 file3 =
  let contents = both (read_file ~path:file1) (read_file ~path:file2) in
  map contents ~f:(fun (data1, data2) ->
      write_file ~path:file3 ~data:(String.concat [ data1; data2 ]))

let () = run @@ concatenate_files "foo" "bar" "foobar"
