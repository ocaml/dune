  $ cat > dune-project << EOF
  > (lang dune 3.25)
  > (using action-plugin 0.1)
  > EOF

  $ cat > dune << EOF
  > (rule
  >  (targets
  >   (dir output)
  >   (dir second))
  >  (action
  >   (dynamic-run ./foo.exe)))
  > EOF

  $ cp ./bin/foo.exe ./

  $ dune build output second

  $ cat _build/default/output/root.txt
  root

  $ cat _build/default/output/nested/file.txt
  nested

  $ cat _build/default/second/file.txt
  second
