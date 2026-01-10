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

Also creating an alias called empty is allowed prior to 3.20:
  $ cat > dune <<EOF
  > (alias
  >  (name empty)
  >  (deps foo))
  > EOF
  $ cat > foo

  $ dune build @empty

For versions 3.20 and after these should fail:

  $ cat > dune-project <<EOF
  > (lang dune 3.20)
  > EOF

  $ cat > dune <<EOF
  > (rule
  >  (alias empty)
  >  (action
  >   (echo "I should not be added!")))
  > EOF

  $ dune build @empty
  File "dune", lines 1-4, characters 0-65:
  1 | (rule
  2 |  (alias empty)
  3 |  (action
  4 |   (echo "I should not be added!")))
  Error: User-defined rules cannot be added to the 'empty' alias
  [1]

  $ cat > dune <<EOF
  > (alias
  >  (name empty)
  >  (deps foo))
  > EOF

  $ dune build @empty
  File "dune", lines 1-3, characters 0-33:
  1 | (alias
  2 |  (name empty)
  3 |  (deps foo))
  Error: User-defined rules cannot be added to the 'empty' alias
  [1]

