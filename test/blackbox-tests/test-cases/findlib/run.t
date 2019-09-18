  $ dune external-lib-deps @install
  These are the external library dependencies in the default context:
  - a
  - b
  - c

Reproduction case for #484. The error should point to src/dune

  $ dune build @install
  File "src/dune", line 4, characters 18-19:
  4 |  (libraries   a b c))
                        ^
  Error: Library "c" not found.
  Hint: try: dune external-lib-deps --missing @install
  [1]

Note that the hint above is wrong. It doesn't matter too much as this
is for jbuilder which is deprecated and it doesn't seem worth making
the code of dune more complicated to fix the hint.

With dune and an explicit profile, it is the same:

  $ dune build --profile dev @install
  File "src/dune", line 4, characters 18-19:
  4 |  (libraries   a b c))
                        ^
  Error: Library "c" not found.
  Hint: try: dune external-lib-deps --missing --profile dev @install
  [1]
