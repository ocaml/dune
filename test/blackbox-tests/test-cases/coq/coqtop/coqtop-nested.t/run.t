Checking that we compute the directory and file for dune coq top correctly

  $ dune build theories/c.vo
  $ dune build theories/b/b.vo
  $ dune coq top --toplevel=echo theories/c.v
  -topfile $TESTCASE_ROOT/_build/default/theories/c.v -q -w -deprecated-native-compiler-option -w -native-compiler-disabled -native-compiler ondemand -R $TESTCASE_ROOT/_build/default/theories foo
  $ dune coq top --toplevel=echo theories/b/b.v
  -topfile $TESTCASE_ROOT/_build/default/theories/b/b.v -q -w -deprecated-native-compiler-option -w -native-compiler-disabled -native-compiler ondemand -R $TESTCASE_ROOT/_build/default/theories foo
