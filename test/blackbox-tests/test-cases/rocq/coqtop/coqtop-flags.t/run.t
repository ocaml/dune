Testing that the correct flags are being passed to dune rocq top

The flags passed to coqc:
  $ dune build && tail -1 _build/log | ../../scrub_coq_args.sh
  coqc
  -w -notation-overridden
  -w -deprecated-native-compiler-option
  -w -native-compiler-disabled
  -native-compiler ondemand
  -boot
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
  -R coq/theories Corelib
  -R $TESTCASE_ROOT/_build/default minimal
