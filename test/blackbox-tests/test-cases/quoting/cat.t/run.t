  $ cat > a
  $ cat > b

It should be possible to expand %{deps} in a cat action since it allows multiple
arguments.

  $ cat > dune <<EOF
  > (rule
  >  (alias foo)
  >  (deps a b)
  >  (action
  >   (cat %{deps})))
  > EOF

  $ dune build @foo
  File "dune", line 5, characters 7-14:
  5 |   (cat %{deps})))
             ^^^^^^^
  Error: Variable %{deps} expands to 2 values, however a single value is
  expected here. Please quote this atom.
  [1]
