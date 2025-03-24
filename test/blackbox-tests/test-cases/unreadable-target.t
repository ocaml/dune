  $ cat > dune-project <<EOF
  > (lang dune 2.9)
  > EOF

  $ cat > dune <<EOF
  > (rule
  >   (targets a b)
  >   (action (bash "echo content > a; chmod -r a; ln -s foo b")))
  > EOF

  $ dune build b
  File "dune", lines 1-3, characters 0-84:
  1 | (rule
  2 |   (targets a b)
  3 |   (action (bash "echo content > a; chmod -r a; ln -s foo b")))
  Error: Error trying to read targets after a rule was run:
  - a: Permission denied
  - b: Broken symbolic link
  [1]
