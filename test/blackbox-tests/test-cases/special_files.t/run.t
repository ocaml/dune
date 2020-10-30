  $ mkfifo src/foo
  $ dune build @all
  $ _build/default/bar.exe
  hi!
  $ rm -f src/foo

  $ ln -s broken-link src/foo
  $ dune build @all
  File "src/dune", line 1, characters 0-0:
  Error: File unavailable: src/foo
  [1]
  $ _build/default/bar.exe
  hi!
  $ rm -f src/foo
