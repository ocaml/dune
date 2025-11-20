Checking that we compute the directory and file for dune coq top correctly

  $ dune build theories/c.vo
  Warning: Coq Language Versions lower than 0.8 have been deprecated in Dune
  3.8 and will be removed in an upcoming Dune version.
  Hint: To disable this warning, add the following to your dune-project file:
  (warnings (deprecated_coq_lang_lt_08 disabled))
  $ dune build theories/b/b.vo
  Warning: Coq Language Versions lower than 0.8 have been deprecated in Dune
  3.8 and will be removed in an upcoming Dune version.
  Hint: To disable this warning, add the following to your dune-project file:
  (warnings (deprecated_coq_lang_lt_08 disabled))
  $ dune coq top --toplevel=echo theories/c.v | ../../scrub_coq_args.sh
  Warning: Coq Language Versions lower than 0.8 have been deprecated in Dune
  3.8 and will be removed in an upcoming Dune version.
  Hint: To disable this warning, add the following to your dune-project file:
  (warnings (deprecated_coq_lang_lt_08 disabled))
  -topfile $TESTCASE_ROOT/_build/default/theories/c.v
  -w -deprecated-native-compiler-option
  -w -native-compiler-disabled
  -native-compiler ondemand
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
  -R $TESTCASE_ROOT/_build/default/theories foo

  $ dune coq top --toplevel=echo theories/b/b.v | ../../scrub_coq_args.sh
  Warning: Coq Language Versions lower than 0.8 have been deprecated in Dune
  3.8 and will be removed in an upcoming Dune version.
  Hint: To disable this warning, add the following to your dune-project file:
  (warnings (deprecated_coq_lang_lt_08 disabled))
  -topfile $TESTCASE_ROOT/_build/default/theories/b/b.v
  -w -deprecated-native-compiler-option
  -w -native-compiler-disabled
  -native-compiler ondemand
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
  -R $TESTCASE_ROOT/_build/default/theories foo

