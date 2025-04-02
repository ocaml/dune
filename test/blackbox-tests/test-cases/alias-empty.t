Testing the empty alias

  $ cat > dune-project <<EOF
  > (lang dune 3.17)
  > EOF

Building the empty alias does nothing
  $ dune build @empty

We should check that a user hasn't added anything to an empty alias
  $ cat > dune <<EOF
  > (rule
  >  (alias empty)
  >  (action
  >   (echo "I shouldn't be added!")))
  > EOF

This fails as expected
  $ dune build @empty
  File "dune", lines 1-4, characters 0-64:
  1 | (rule
  2 |  (alias empty)
  3 |  (action
  4 |   (echo "I shouldn't be added!")))
  Error: User-defined rules cannot be added to the 'empty' alias
  [1]
