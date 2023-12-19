  $ mkfifo src/foo
  $ dune build @all
  $ _build/default/bar.exe
  hi!
  $ rm -f src/foo

Check the error message Dune produces when it encounters a broken
symlink:

  $ ln -s broken-link src/foo
  $ dune build @all
  Error: File unavailable: src/foo
  Broken symbolic link
  -> required by _build/default/src/foo
  -> required by alias src/all
  [1]
  $ _build/default/bar.exe
  hi!
  $ rm -f src/foo
