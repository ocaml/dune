This test demonstrates that an incorrect implementation prevents all rules from
being loaded in the same directory

  $ cat >dune-project <<EOF
  > (lang dune 3.7)
  > EOF

We define an invalid library along with a rule and an executable that should be
buildable despite the presence of this library

  $ cat >dune <<EOF
  > (library
  >  (name foo)
  >  (modules :standard \ foo)
  >  (implements fake-dummy))
  > (rule (with-stdout-to test (echo foo)))
  > (executable
  >  (name foo)
  >  (modules foo))
  > EOF

  $ touch foo.ml
  $ dune build ./test
  File "dune", line 4, characters 13-23:
  4 |  (implements fake-dummy))
                   ^^^^^^^^^^
  Error: Library "fake-dummy" not found.
  [1]
  $ dune build ./foo.exe
  File "dune", line 4, characters 13-23:
  4 |  (implements fake-dummy))
                   ^^^^^^^^^^
  Error: Library "fake-dummy" not found.
  [1]
