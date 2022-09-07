  $ cat >dune-project <<EOF
  > (lang dune 3.0)
  > EOF

  $ cat >dune <<EOF
  > (test
  >  (name missing_test)
  >  (modules missing_test)
  >  (enabled_if false)
  >  (libraries lib_not))
  > 
  > (library
  >  (name lib_not)
  >  (modules lib_not)
  >  (enabled_if false))
  > EOF
  $ touch missing_test.ml

The test [missing_test] depends on a library that is disabled, but that should
not trigger any error since the test itself is disabled in the same way as the
library it depends on.
  $ dune build @all
