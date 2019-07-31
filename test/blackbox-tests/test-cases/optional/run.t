Various tests for optional libraries
------------------------------------

Regression test for non-wrapped optional libraries with missing
dependencies (#1281):

  $ dune build @install

Interaction between `@all` and optional libraries:

  $ dune build @all
