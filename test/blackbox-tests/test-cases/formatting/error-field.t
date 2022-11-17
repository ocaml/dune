  $ cat > dune-project << EOF
  > (lang dune 3.5)
  > EOF

  $ cat > dune << EOF
  > (env
  >  (_
  >   (formatting (enabled-with-typo a b c))))
  > EOF

  $ dune build @fmt
  File "dune", line 3, characters 15-32:
  3 |   (formatting (enabled-with-typo a b c))))
                     ^^^^^^^^^^^^^^^^^
  Error: Unknown field enabled-with-typo
  [1]
