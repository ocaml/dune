  $ cat > dune-project << EOF
  > (lang dune 2.0)
  > (using action-plugin 0.1)
  > EOF

  $ cat > dune << EOF
  > (rule
  >  (target some_file)
  >  (action
  >   (dynamic-run ./foo.exe)))
  > EOF

  $ cp ./bin/foo.exe ./

Reading a directory containing the action's target currently deadlocks.

  $ timeout 3 dune build some_file
  [124]
