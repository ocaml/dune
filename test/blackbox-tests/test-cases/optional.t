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

Reproduction case for a bug in dune < 2.4 where all libraries where
considered as optional:

  $ cat >dune <<EOF
  > (library
  >  (name foo)
  >  (public_name foo)
  >  (libraries lib_that_doesn't_exist))
  > EOF

The following command should fail because the executable is not optional:

  $ dune build @install
  File "dune", line 4, characters 12-34:
  4 |  (libraries lib_that_doesn't_exist))
                  ^^^^^^^^^^^^^^^^^^^^^^
  Error: Library "lib_that_doesn't_exist" not found.
  -> required by library "foo" in _build/default
  -> required by _build/default/META.foo
  -> required by _build/install/default/lib/foo/META
  -> required by _build/default/foo.install
  -> required by alias install
  [1]
