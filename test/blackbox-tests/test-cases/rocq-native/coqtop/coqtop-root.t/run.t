All dune commands work when you run them in sub-directories, so this should be no exception.

  $ dune rocq top --toplevel=echo -- theories/foo.v | ../../scrub_coq_args.sh
  -topfile $TESTCASE_ROOT/_build/default/theories/foo.v
  -w -deprecated-native-compiler-option -native-output-dir .
  -native-compiler on
  -nI lib/rocq-runtime/kernel
  -nI $TESTCASE_ROOT/_build/default/theories
  -boot
  -R coq/theories Corelib
  -R $TESTCASE_ROOT/_build/default/theories foo
  $ cd theories

This test is currently broken due to the workspace resolution being faulty #5899.
  $ dune rocq top --toplevel=echo -- foo.v
  File ".", line 1, characters 0-0:
  Warning: No dune-project file has been found in directory ".". A default one
  is assumed but the project might break when dune is upgraded. Please create a
  dune-project file.
  Hint: generate the project file with: $ dune init project <name>
  File "dune", lines 1-2, characters 0-25:
  1 | (rocq.theory
  2 |  (name foo))
  Error: 'rocq.theory' is available only when rocq is enabled in the
  dune-project file. You must enable it using (using rocq 0.11) in your
  dune-project file.
  [1]
