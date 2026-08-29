  $ cat > dune-project << EOF
  > (lang dune 2.0)
  > (using action-plugin 0.1)
  > EOF

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
  > \
  > (rule
  >  (target some_file3)
  >  (action
  >   (dynamic-run ./foo1.exe some_file4)))
  > \
  > (rule
  >  (target some_file4)
  >  (deps some_file3)
  >  (action
  >   (write-file some_file4 done)))
  > EOF

  $ cp ./bin/foo1.exe ./
  $ cp ./bin/foo2.exe ./

Direct dependencies on the action's target are rejected.

  $ timeout 3 dune build some_file1
  File "dune", lines 1-4, characters 0-64:
  1 | (rule
  2 |  (target some_file1)
  3 |  (action
  4 |   (dynamic-run ./foo1.exe)))
  Dependency cycle between:
     _build/default/some_file1
  [1]

  $ timeout 3 dune build some_file2
  File "dune", lines 5-8, characters 0-64:
  5 | (rule
  6 |  (target some_file2)
  7 |  (action
  8 |   (dynamic-run ./foo2.exe)))
  Dependency cycle between:
     _build/default/some_file2
  [1]

An indirect dependency on the action's target is rejected too.

  $ timeout 3 dune build some_file3
  File "dune", lines 9-12, characters 0-75:
   9 | (rule
  10 |  (target some_file3)
  11 |  (action
  12 |   (dynamic-run ./foo1.exe some_file4)))
  Dependency cycle between:
     _build/default/some_file4
  -> _build/default/some_file3
  -> _build/default/some_file4
  [1]
