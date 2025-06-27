All dune commands work when you run them in sub-directories, so this should be no exception.

  $ dune rocq top --toplevel=echo -- theories/foo.v | ../../scrub_coq_args.sh
  -topfile $TESTCASE_ROOT/_build/default/theories/foo.v
  -w -deprecated-native-compiler-option
  -w -native-compiler-disabled
  -native-compiler ondemand
  -boot
  -I lib/coq/../rocq-runtime/plugins/btauto
  -I lib/coq/../rocq-runtime/plugins/cc
  -I lib/coq/../rocq-runtime/plugins/cc_core
  -I lib/coq/../rocq-runtime/plugins/derive
  -I lib/coq/../rocq-runtime/plugins/extraction
  -I lib/coq/../rocq-runtime/plugins/firstorder
  -I lib/coq/../rocq-runtime/plugins/firstorder_core
  -I lib/coq/../rocq-runtime/plugins/funind
  -I lib/coq/../rocq-runtime/plugins/ltac
  -I lib/coq/../rocq-runtime/plugins/ltac2
  -I lib/coq/../rocq-runtime/plugins/ltac2_ltac1
  -I lib/coq/../rocq-runtime/plugins/micromega
  -I lib/coq/../rocq-runtime/plugins/micromega_core
  -I lib/coq/../rocq-runtime/plugins/nsatz
  -I lib/coq/../rocq-runtime/plugins/nsatz_core
  -I lib/coq/../rocq-runtime/plugins/number_string_notation
  -I lib/coq/../rocq-runtime/plugins/ring
  -I lib/coq/../rocq-runtime/plugins/rtauto
  -I lib/coq/../rocq-runtime/plugins/ssreflect
  -I lib/coq/../rocq-runtime/plugins/ssrmatching
  -I lib/coq/../rocq-runtime/plugins/tauto
  -I lib/coq/../rocq-runtime/plugins/tutorial/p0
  -I lib/coq/../rocq-runtime/plugins/tutorial/p1
  -I lib/coq/../rocq-runtime/plugins/tutorial/p2
  -I lib/coq/../rocq-runtime/plugins/tutorial/p3
  -I lib/coq/../rocq-runtime/plugins/tutorial/p4
  -I lib/coq/../rocq-runtime/plugins/zify
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
