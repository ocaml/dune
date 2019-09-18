  $ echo "(lang dune 2.0)" > dune-project

  $ cat > dune << EOF
  > (rule
  >  (target some_file1)
  >  (action
  >   (dynamic-run ./foo1.exe)))
  > \
  > (rule
  >  (target some_file2)
  >  (action
  >   (dynamic-run ./foo2.exe)))
  > EOF

  $ cp ../bin/foo1.exe ./
  $ cp ../bin/foo2.exe ./

  $ dune build some_file1 --display short
          foo1 some_file1
  Error: Dependency cycle between the following files:
     _build/default/some_file1
  [1]

  $ dune build some_file2 --display short
          foo2 some_file2
  Error: Dependency cycle between the following files:
     _build/default/some_file2
  [1]
