Checking that we compute the directory and file for dune rocq top correctly

  $ dune build theories/c.vo
  $ dune build theories/b/b.vo
  $ dune rocq top --toplevel=echo theories/c.v | ../../scrub_coq_args.sh
  -topfile $TESTCASE_ROOT/_build/default/theories/c.v
  -w -deprecated-native-compiler-option
  -w -native-compiler-disabled
  -native-compiler ondemand
  -boot
  -R coq/theories Corelib
  -R $TESTCASE_ROOT/_build/default/theories foo

  $ dune rocq top --toplevel=echo theories/b/b.v | ../../scrub_coq_args.sh
  -topfile $TESTCASE_ROOT/_build/default/theories/b/b.v
  -w -deprecated-native-compiler-option
  -w -native-compiler-disabled
  -native-compiler ondemand
  -boot
  -R coq/theories Corelib
  -R $TESTCASE_ROOT/_build/default/theories foo

