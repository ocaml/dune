Testing that the correct flags are being passed to dune coq top

The flags passed to coqc:
  $ dune build && tail -1 _build/log | sed 's/(cd .*coqc/coqc/' | sed 's/$ //'
  coqc -w -notation-overridden -w -deprecated-native-compiler-option -w -native-compiler-disabled -native-compiler ondemand -R . minimal Test.v)

BUG: coqtop flags are missing. Specifically ones from Context.coq_flags in
coq_rules.ml

The flags passed to coqtop:
  $ dune coq top --toplevel=echo Test.v
  -topfile $TESTCASE_ROOT/_build/default/Test.v -R $TESTCASE_ROOT/_build/default minimal
