Dune should warn about duplicate modules declared inside a coq.theory stanza

  $ cat > dune << EOF
  > (coq.theory
  >  (name foo)
  >  (modules foo bar foo))
  > EOF

  $ dune build
  File "dune", line 1, characters 0-47:
  1 | (coq.theory
  2 |  (name foo)
  3 |  (modules foo bar foo))
  Error: Duplicate Coq module "foo".
  [1]
