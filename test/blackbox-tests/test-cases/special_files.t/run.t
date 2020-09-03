  $ mkfifo src/foo
  $ dune build @all
  $ _build/default/bar.exe
  hi!
  $ rm -f src/foo
