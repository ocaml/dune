Dune should warn about duplicate modules declared inside a rocq.theory stanza

  $ cat > dune << EOF
  > (rocq.theory
  >  (name foo)
  >  (modules foo bar foo))
  > EOF

  $ dune build
  File "dune", lines 1-3, characters 0-48:
  1 | (rocq.theory
  2 |  (name foo)
  3 |  (modules foo bar foo))
  Error: Duplicate Rocq module "foo".
  [1]
