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
  -I lib/coq/../coq-core/plugins/btauto
  -I lib/coq/../coq-core/plugins/cc
  -I lib/coq/../coq-core/plugins/derive
  -I lib/coq/../coq-core/plugins/extraction
  -I lib/coq/../coq-core/plugins/firstorder
  -I lib/coq/../coq-core/plugins/funind
  -I lib/coq/../coq-core/plugins/ltac
  -I lib/coq/../coq-core/plugins/ltac2
  -I lib/coq/../coq-core/plugins/micromega
  -I lib/coq/../coq-core/plugins/nsatz
  -I lib/coq/../coq-core/plugins/number_string_notation
  -I lib/coq/../coq-core/plugins/ring
  -I lib/coq/../coq-core/plugins/rtauto
  -I lib/coq/../coq-core/plugins/ssreflect
  -I lib/coq/../coq-core/plugins/ssrmatching
  -I lib/coq/../coq-core/plugins/tauto
  -I lib/coq/../coq-core/plugins/tutorial/p0
  -I lib/coq/../coq-core/plugins/tutorial/p1
  -I lib/coq/../coq-core/plugins/tutorial/p2
  -I lib/coq/../coq-core/plugins/tutorial/p3
  -I lib/coq/../coq-core/plugins/zify
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
  -I lib/coq/../coq-core/plugins/btauto
  -I lib/coq/../coq-core/plugins/cc
  -I lib/coq/../coq-core/plugins/derive
  -I lib/coq/../coq-core/plugins/extraction
  -I lib/coq/../coq-core/plugins/firstorder
  -I lib/coq/../coq-core/plugins/funind
  -I lib/coq/../coq-core/plugins/ltac
  -I lib/coq/../coq-core/plugins/ltac2
  -I lib/coq/../coq-core/plugins/micromega
  -I lib/coq/../coq-core/plugins/nsatz
  -I lib/coq/../coq-core/plugins/number_string_notation
  -I lib/coq/../coq-core/plugins/ring
  -I lib/coq/../coq-core/plugins/rtauto
  -I lib/coq/../coq-core/plugins/ssreflect
  -I lib/coq/../coq-core/plugins/ssrmatching
  -I lib/coq/../coq-core/plugins/tauto
  -I lib/coq/../coq-core/plugins/tutorial/p0
  -I lib/coq/../coq-core/plugins/tutorial/p1
  -I lib/coq/../coq-core/plugins/tutorial/p2
  -I lib/coq/../coq-core/plugins/tutorial/p3
  -I lib/coq/../coq-core/plugins/zify
  -R coq/theories Coq
  -R $TESTCASE_ROOT/_build/default minimal
