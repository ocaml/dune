let () =
  Sys.mkdir "fakenode_modules" 0o777;
  Sys.mkdir "fakenode_modules/foo" 0o777;
  Unix.symlink "file" "./fakenode_modules/foo/bar"
