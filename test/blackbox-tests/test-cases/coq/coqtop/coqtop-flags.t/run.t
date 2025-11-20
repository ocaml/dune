Testing that the correct flags are being passed to dune coq top

The flags passed to coqc:
  $ dune build && tail -1 _build/log | ../../scrub_coq_args.sh
  Warning: Coq Language Versions lower than 0.8 have been deprecated in Dune
  3.8 and will be removed in an upcoming Dune version.
  Hint: To disable this warning, add the following to your dune-project file:
  (warnings (deprecated_coq_lang_lt_08 disabled))
  coqc
  -w -notation-overridden
  -w -deprecated-native-compiler-option -native-output-dir .
  -native-compiler on
  -nI lib/coq-core/kernel
  -nI .
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
  -R . minimal
  Test.v

The flags passed to coqtop:
  $ dune coq top --toplevel=echo Test.v | ../../scrub_coq_args.sh
  Warning: Coq Language Versions lower than 0.8 have been deprecated in Dune
  3.8 and will be removed in an upcoming Dune version.
  Hint: To disable this warning, add the following to your dune-project file:
  (warnings (deprecated_coq_lang_lt_08 disabled))
  -topfile $TESTCASE_ROOT/_build/default/Test.v
  -w -notation-overridden
  -w -deprecated-native-compiler-option -native-output-dir .
  -native-compiler on
  -nI lib/coq-core/kernel
  -nI $TESTCASE_ROOT/_build/default
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
  -R $TESTCASE_ROOT/_build/default minimal
