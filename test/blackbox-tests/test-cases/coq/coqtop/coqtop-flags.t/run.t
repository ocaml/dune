Testing that the correct flags are being passed to dune coq top

The flags passed to coqc:
  $ dune build && tail -1 _build/log | ../../scrub_coq_args.sh
  coqc
  -w -notation-overridden
  -w -deprecated-native-compiler-option -native-output-dir . -native-compiler on
  -I coq-core/kernel
  -nI .
  -R . minimal Test.v)

The flags passed to coqtop:
  $ dune coq top --toplevel=echo Test.v | ../../scrub_coq_args.sh
  -topfile $TESTCASE_ROOT/_build/default/Test.v
  -w -notation-overridden
  -w -deprecated-native-compiler-option -native-output-dir . -native-compiler on
  -I coq-core/kernel
  -nI $TESTCASE_ROOT/_build/default
  -R coqtop-flags.t/_build/default minimal
