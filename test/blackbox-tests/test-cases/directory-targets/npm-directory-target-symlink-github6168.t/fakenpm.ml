let () =
  Sys.mkdir "fakenode_modules" 0o777;
  Sys.mkdir "fakenode_modules/foo" 0o777;
  open_out "fakenode_modules/foo/file" |> close_out;
  Unix.symlink "file" "./fakenode_modules/foo/bar"
;;
