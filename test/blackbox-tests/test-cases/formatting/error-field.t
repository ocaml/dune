Rejects unknown fields in `(formatting ...)`.

  $ make_dune_project 3.5

  $ cat > dune << EOF
  > (env
  >  (_
  >   (formatting (enabled-with-typo a b c))))
  > EOF

  $ dune build @fmt
  File "dune", line 3, characters 15-32:
  3 |   (formatting (enabled-with-typo a b c))))
                     ^^^^^^^^^^^^^^^^^
  Error: Unknown field "enabled-with-typo"
  [1]
