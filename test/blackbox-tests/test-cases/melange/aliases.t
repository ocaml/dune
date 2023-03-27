Testing the alias and aliases field for melange.emit

  $ cat > dune-project <<EOF
  > (lang dune 3.8)
  > (using melange 0.1)
  > EOF

  $ cat > dune <<EOF
  > (melange.emit
  >  (target output)
  >  (alias app))
  > EOF

  $ cat > main.ml <<EOF
  > let () =
  >   print_endline "hello"
  > EOF

  $ dune build @app
  $ node _build/default/output/main.js
  hello

  $ dune clean

The aliases field lets you define multiple aliases.

  $ cat > dune <<EOF
  > (melange.emit
  >  (target output)
  >  (aliases app lication))
  > EOF

  $ ls
  dune
  dune-project
  main.ml
  $ dune build @lication
  $ ls _build/default/output/main.js
  _build/default/output/main.js

  $ dune clean

Both the alias and the aliases field cannot be used at the same time.

  $ cat > dune <<EOF
  > (melange.emit
  >  (target output)
  >  (alias app)
  >  (aliases lication))
  > EOF

  $ dune build @app
  File "dune", line 1, characters 0-64:
  1 | (melange.emit
  2 |  (target output)
  3 |  (alias app)
  4 |  (aliases lication))
  Error: The 'alias' and 'aliases' fields are mutually exclusive. Please use
  only the 'aliases' field.
  [1]
