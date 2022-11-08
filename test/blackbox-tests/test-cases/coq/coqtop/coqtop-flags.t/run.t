Testing that the correct flags are being passed to dune coq top

The flags passed to coqc:
  $ dune build && tail -1 _build/log | sed 's/(cd .*coqc/coqc/' | sed 's/$ //' | sed 's/-nI .*coq-core/some-coq-core/'
  coqc -w -notation-overridden -w -deprecated-native-compiler-option -native-output-dir . -native-compiler on some-coq-core/kernel -nI . -R . minimal Test.v)

The flags passed to coqtop:
  $ dune coq top --toplevel=echo Test.v | sed 's/-nI .*coq-core/some-coq-core/'
  -topfile $TESTCASE_ROOT/_build/default/Test.v -w -notation-overridden -w -deprecated-native-compiler-option -native-output-dir . -native-compiler on some-coq-core/kernel -nI $TESTCASE_ROOT/_build/default -R $TESTCASE_ROOT/_build/default minimal
