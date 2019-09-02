  $ cp ../bin/foo1.exe ./
  $ cp ../bin/foo2.exe ./
  $ dune build bar1 --display short
          foo1 bar1
  Error: Dependency cycle between the following files:
     _build/default/bar1
  [1]
  $ dune build bar2 --display short
          foo2 bar2
  Error: Dependency cycle between the following files:
     _build/default/bar2
  [1]
