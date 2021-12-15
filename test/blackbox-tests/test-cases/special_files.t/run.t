  $ mkfifo src/foo
  $ dune build @all
  $ _build/default/bar.exe
  hi!
  $ rm -f src/foo

Check the error message Dune produces when it encounters a broken
symlink:

  $ ln -s broken-link src/foo
  $ dune build @all
  File "src/foo", line 1, characters 0-0:
  Error: File unavailable: src/foo
  Broken symbolic link
  [1]
  $ _build/default/bar.exe
  hi!
  $ rm -f src/foo
