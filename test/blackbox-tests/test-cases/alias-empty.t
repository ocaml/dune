Testing the empty alias

  $ cat > dune-project <<EOF
  > (lang dune 3.17)
  > EOF

Building the empty alias does nothing
  $ dune build @empty

We should check that a user has not added anything to an empty alias:
  $ cat > dune <<EOF
  > (rule
  >  (alias empty)
  >  (action
  >   (echo "I should not be added!")))
  > EOF

For versions prior to 3.20 this does not fail:
  $ dune build @empty
  I should not be added!

For versions 3.20 and after this fails:
  $ cat > dune-project <<EOF
  > (lang dune 3.20)
  > EOF

  $ dune build @empty
  File "dune", lines 1-4, characters 0-65:
  1 | (rule
  2 |  (alias empty)
  3 |  (action
  4 |   (echo "I should not be added!")))
  Error: User-defined rules cannot be added to the 'empty' alias
  [1]

