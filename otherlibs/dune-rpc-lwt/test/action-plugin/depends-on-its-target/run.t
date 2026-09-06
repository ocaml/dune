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
  > EOF

  $ cp ./bin/foo1.exe ./
  $ cp ./bin/foo2.exe ./

Direct dependencies on the action's target currently deadlock.

  $ timeout 3 dune build some_file1
  [124]

  $ timeout 3 dune build some_file2
  [124]
