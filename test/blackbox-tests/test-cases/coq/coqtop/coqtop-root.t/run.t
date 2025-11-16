All dune commands work when you run them in sub-directories, so this should be no exception.

  $ dune coq top --toplevel=echo -- theories/foo.v | ../../scrub_coq_args.sh
  Warning: Coq Language Versions lower than 0.8 have been deprecated in Dune
  3.8 and will be removed in an upcoming Dune version.
  Hint: To disable this warning, add the following to your dune-project file:
  (warnings (deprecated_coq_lang_lt_08 disabled))
  -topfile $TESTCASE_ROOT/_build/default/theories/foo.v
  -w -deprecated-native-compiler-option
  -w -native-compiler-disabled
  -native-compiler ondemand
  -I .../plugins/btauto
  -I .../plugins/cc
  -I .../plugins/derive
  -I .../plugins/extraction
  -I .../plugins/firstorder
  -I .../plugins/funind
  -I .../plugins/ltac
  -I .../plugins/ltac2
  -I .../plugins/micromega
  -I .../plugins/nsatz
  -I .../plugins/number_string_notation
  -I .../plugins/ring
  -I .../plugins/rtauto
  -I .../plugins/ssreflect
  -I .../plugins/ssrmatching
  -I .../plugins/tauto
  -I .../plugins/tutorial/p0
  -I .../plugins/tutorial/p1
  -I .../plugins/tutorial/p2
  -I .../plugins/tutorial/p3
  -I .../plugins/zify
  -R coq/theories Coq
  -R $TESTCASE_ROOT/_build/default/theories foo
  $ cd theories

This test is currently broken due to the workspace resolution being faulty #5899.
  $ dune coq top --toplevel=echo -- foo.v
  File ".", line 1, characters 0-0:
  Warning: No dune-project file has been found in directory ".". A default one
  is assumed but the project might break when dune is upgraded. Please create a
  dune-project file.
  Hint: generate the project file with: $ dune init project <name>
  File "dune", lines 1-2, characters 0-24:
  1 | (coq.theory
  2 |  (name foo))
  Error: 'coq.theory' is available only when coq is enabled in the dune-project
  file. You must enable it using (using coq 0.11) in your dune-project file.
  [1]
