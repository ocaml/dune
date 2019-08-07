Various tests for optional libraries
------------------------------------

Regression test for non-wrapped optional libraries with missing
dependencies (#1281):

  $ dune build @install

Interaction between `@all` and optional libraries:

  $ dune build @all
  File "dune", line 6, characters 12-34:
  6 |  (libraries lib_that_doesn't_exist))
                  ^^^^^^^^^^^^^^^^^^^^^^
  Error: Library "lib_that_doesn't_exist" not found.
  Hint: try: dune external-lib-deps --missing @all
  [1]
