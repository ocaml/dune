Testing that the correct flags are being passed to dune coq top

The flags passed to coqc:
  $ dune build && tail -1 _build/log | sed 's/(cd .*coqc/coqc/' | sed 's/$ //'
  coqc -w -notation-overridden -w -deprecated-native-compiler-option -w -native-compiler-disabled -native-compiler ondemand -R . minimal Test.v)

The flags passed to coqtop:
  $ dune coq top --toplevel=echo Test.v | sed 's/-nI .*coq-core/some-coq-core/'
  -topfile $TESTCASE_ROOT/_build/default/Test.v -w -notation-overridden -w -deprecated-native-compiler-option -w -native-compiler-disabled -native-compiler ondemand -R $TESTCASE_ROOT/_build/default minimal
