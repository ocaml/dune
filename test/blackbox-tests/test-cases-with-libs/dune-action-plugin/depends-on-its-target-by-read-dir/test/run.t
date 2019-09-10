  $ cp ../bin/foo.exe ./
  $ dune build bar --display short
           foo bar
  Error: Dependency cycle between the following files:
     _build/default/bar
  [1]
