All dune commands work when you run them in sub-directories, so this should be no exception.

  $ dune coq top --toplevel=echo -- theories/foo.v
  -topfile $TESTCASE_ROOT/_build/default/theories/foo.v -R $TESTCASE_ROOT/_build/default/theories foo
  $ cd theories

This test is currently broken due to the workspace resolution being faulty #5899.
  $ dune coq top --toplevel=echo -- foo.v
  Warning: No dune-project file has been found. A default one is assumed but
  the project might break when dune is upgraded. Please create a dune-project
  file.
  Hint: generate the project file with: $ dune init project <name>
  File "dune", line 1, characters 0-24:
  1 | (coq.theory
  2 |  (name foo))
  Error: 'coq.theory' is available only when coq is enabled in the dune-project
  file. You must enable it using (using coq 0.6) in your dune-project file.
  [1]
