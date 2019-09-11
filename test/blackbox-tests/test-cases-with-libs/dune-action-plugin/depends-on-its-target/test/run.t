  $ echo "(lang dune 2.0)" > dune-project

  $ cat > dune << EOF
  > (rule
  >  (target bar1)
  >  (action
  >   (dynamic-run ./foo1.exe)))
  > \
  > (rule
  >  (target bar2)
  >  (action
  >   (dynamic-run ./foo2.exe)))
  > EOF

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
