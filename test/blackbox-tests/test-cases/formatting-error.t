  $ cat > dune-project << EOF
  > (lang dune 3.5)
  > EOF

  $ cat > dune << EOF
  > (library
  >  (name l))
  > 
  > (env
  >  (_
  >   (formatting (enabled-with-typo a b c))))
  > EOF

  $ touch .ocamlformat

  $ cat > a.ml << EOF
  > let x = 1
  > EOF

  $ cp a.ml b.ml

  $ dune build @fmt
  File "dune", line 6, characters 15-32:
  6 |   (formatting (enabled-with-typo a b c))))
                     ^^^^^^^^^^^^^^^^^
  Error: Unknown field enabled-with-typo
  [1]
