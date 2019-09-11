  $ echo "(lang dune 2.0)" > dune-project

  $ cat > dune << EOF
  > (rule
  >  (target bar)
  >  (action
  >   (dynamic-run ./foo.exe)))
  > EOF

  $ cp ../bin/foo.exe ./

  $ dune build bar --display short
           foo bar
  Error: Dependency cycle between the following files:
     _build/default/bar
  [1]
