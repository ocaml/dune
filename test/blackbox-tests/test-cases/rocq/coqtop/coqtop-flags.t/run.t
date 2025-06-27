Testing that the correct flags are being passed to dune rocq top

The flags passed to coqc:
  $ dune build && tail -1 _build/log | ../../scrub_coq_args.sh
  coqc
  -w -notation-overridden
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
  -R . minimal
  Test.v

The flags passed to coqtop:
  $ dune rocq top --toplevel=echo Test.v | ../../scrub_coq_args.sh
  -topfile $TESTCASE_ROOT/_build/default/Test.v
  -w -notation-overridden
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
  -R $TESTCASE_ROOT/_build/default minimal
