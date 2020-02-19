Various tests for optional libraries
------------------------------------

  $ cat >dune-project <<EOF
  > (lang dune 1.2)
  > (name foo)
  > (package (name foo))
  > EOF

  $ cat >dune <<EOF
  > (library
  >  (name foo)
  >  (public_name foo)
  >  (wrapped false)
  >  (optional)
  >  (libraries lib_that_doesn't_exist))
  > EOF

  $ cat >x.ml <<EOF
  > let x = 42
  > EOF

Regression test for non-wrapped optional libraries with missing
dependencies (#1281):

  $ dune build @install

Interaction between `@all` and optional libraries:

  $ dune build @all
