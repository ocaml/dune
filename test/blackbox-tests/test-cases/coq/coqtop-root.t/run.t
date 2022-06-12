All dune commands work when you run them in sub-directories, so this should be no exception.

  $ dune coq top --toplevel=echo -- theories/foo.v
  -topfile $TESTCASE_ROOT/_build/default/theories/foo.v -R $TESTCASE_ROOT/_build/default/theories foo
  $ cd theories
  $ dune coq top --root=.. --toplevel=echo -- foo.v
  Entering directory '..'
  Error: cannot find file: foo.v
  Hint: is the file part of a stanza?
  Hint: has the file been written to disk?
  [1]
