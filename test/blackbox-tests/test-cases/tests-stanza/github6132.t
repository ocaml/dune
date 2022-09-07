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

FIXME: both the missing_test and lib_not are disabled so this error could be
ignored.
  $ dune build @all
  File "dune", line 9, characters 10-17:
  9 |  (modules lib_not)
                ^^^^^^^
  Error: Module Lib_not doesn't exist.
  [1]
