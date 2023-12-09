Test cases to check Coq's flag setting is correct:

  $ cat > dune-project <<EOF
  > (lang dune 2.8)
  > (using coq 0.3)
  > EOF

  $ cat > foo.v <<EOF
  > Definition t := 3.
  > EOF

Test case: default flags

  $ cat > dune <<EOF
  > (coq.theory
  >  (name foo))
  > EOF

  $ dune build foo.vo && tail -n 1 _build/log | ./scrub_coq_args.sh
  Warning: Coq Language Versions lower than 0.8 have been deprecated in Dune
  3.8 and will be removed in an upcoming Dune version.
  Hint: To disable this warning, add the following to your dune-project file:
  (warnings (deprecated_coq_lang_lt_08 disabled))
  coqc -q
  -w -deprecated-native-compiler-option
  -w -native-compiler-disabled
  -native-compiler ondemand
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
  -R . foo
  foo.v

TC: :standard

  $ cat > dune <<EOF
  > (coq.theory
  >  (name foo)
  >  (flags :standard))
  > EOF

  $ rm _build/default/foo.vo
  $ dune build foo.vo && tail -n 1 _build/log | ./scrub_coq_args.sh
  Warning: Coq Language Versions lower than 0.8 have been deprecated in Dune
  3.8 and will be removed in an upcoming Dune version.
  Hint: To disable this warning, add the following to your dune-project file:
  (warnings (deprecated_coq_lang_lt_08 disabled))
  coqc -q
  -w -deprecated-native-compiler-option
  -w -native-compiler-disabled
  -native-compiler ondemand
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
  -R . foo
  foo.v

TC: override :standard

  $ cat > dune <<EOF
  > (coq.theory
  >  (name foo)
  >  (flags ))
  > EOF

  $ dune build foo.vo && tail -n 1 _build/log | ./scrub_coq_args.sh
  Warning: Coq Language Versions lower than 0.8 have been deprecated in Dune
  3.8 and will be removed in an upcoming Dune version.
  Hint: To disable this warning, add the following to your dune-project file:
  (warnings (deprecated_coq_lang_lt_08 disabled))
  coqc
  -w -deprecated-native-compiler-option
  -w -native-compiler-disabled
  -native-compiler ondemand
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
  -R . foo
  foo.v

TC: add to :standard

  $ cat > dune <<EOF
  > (coq.theory
  >  (name foo)
  >  (flags :standard -type-in-type))
  > EOF

  $ dune build foo.vo && tail -n 1 _build/log | ./scrub_coq_args.sh
  Warning: Coq Language Versions lower than 0.8 have been deprecated in Dune
  3.8 and will be removed in an upcoming Dune version.
  Hint: To disable this warning, add the following to your dune-project file:
  (warnings (deprecated_coq_lang_lt_08 disabled))
  coqc -q -type-in-type
  -w -deprecated-native-compiler-option
  -w -native-compiler-disabled
  -native-compiler ondemand
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
  -R . foo
  foo.v

TC: extend in workspace + override standard

  $ cat > dune <<EOF
  > (coq.theory
  >  (name foo)
  >  (flags :standard))
  > EOF

  $ cat > dune-workspace <<EOF
  > (lang dune 2.8)
  > (env (dev (coq (flags -type-in-type))))
  > EOF

  $ dune build foo.vo && tail -n 1 _build/log | ./scrub_coq_args.sh
  Warning: Coq Language Versions lower than 0.8 have been deprecated in Dune
  3.8 and will be removed in an upcoming Dune version.
  Hint: To disable this warning, add the following to your dune-project file:
  (warnings (deprecated_coq_lang_lt_08 disabled))
  coqc -type-in-type
  -w -deprecated-native-compiler-option
  -w -native-compiler-disabled
  -native-compiler ondemand
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
  -R . foo
  foo.v

TC: extend in workspace + override standard

  $ cat > dune-workspace <<EOF
  > (lang dune 2.8)
  > (env (dev (coq (flags :standard -type-in-type))))
  > EOF

  $ dune build foo.vo && tail -n 1 _build/log | ./scrub_coq_args.sh
  Warning: Coq Language Versions lower than 0.8 have been deprecated in Dune
  3.8 and will be removed in an upcoming Dune version.
  Hint: To disable this warning, add the following to your dune-project file:
  (warnings (deprecated_coq_lang_lt_08 disabled))
  coqc -q -type-in-type
  -w -deprecated-native-compiler-option
  -w -native-compiler-disabled
  -native-compiler ondemand
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
  -R . foo
  foo.v

TC: extend in dune (env) + override standard

  $ cat > dune <<EOF
  > (coq.theory
  >  (name foo))
  > (env (dev (coq (flags -type-in-type))))
  > EOF

  $ rm -rf _build/default/foo.vo
  $ dune build foo.vo && tail -n 1 _build/log | ./scrub_coq_args.sh
  Warning: Coq Language Versions lower than 0.8 have been deprecated in Dune
  3.8 and will be removed in an upcoming Dune version.
  Hint: To disable this warning, add the following to your dune-project file:
  (warnings (deprecated_coq_lang_lt_08 disabled))
  coqc -type-in-type
  -w -deprecated-native-compiler-option
  -w -native-compiler-disabled
  -native-compiler ondemand
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
  -R . foo
  foo.v

TC: extend in dune (env) + standard

  $ cat > dune <<EOF
  > (coq.theory
  >  (name foo))
  > (env (dev (coq (flags :standard -type-in-type))))
  > EOF

  $ rm -rf _build/default/foo.vo
  $ dune build foo.vo && tail -n 1 _build/log | ./scrub_coq_args.sh
  Warning: Coq Language Versions lower than 0.8 have been deprecated in Dune
  3.8 and will be removed in an upcoming Dune version.
  Hint: To disable this warning, add the following to your dune-project file:
  (warnings (deprecated_coq_lang_lt_08 disabled))
  coqc -q -type-in-type -type-in-type
  -w -deprecated-native-compiler-option
  -w -native-compiler-disabled
  -native-compiler ondemand
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
  -R . foo
  foo.v

TC: extend in dune (env) + workspace + standard

  $ cat > dune <<EOF
  > (coq.theory
  >  (name foo))
  > (env (dev (coq (flags :standard -bt))))
  > EOF

  $ cat > dune-workspace <<EOF
  > (lang dune 2.8)
  > (env (dev (coq (flags :standard -type-in-type))))
  > EOF

  $ rm -rf _build/default/foo.vo
  $ dune build foo.vo && tail -n 1 _build/log | ./scrub_coq_args.sh
  Warning: Coq Language Versions lower than 0.8 have been deprecated in Dune
  3.8 and will be removed in an upcoming Dune version.
  Hint: To disable this warning, add the following to your dune-project file:
  (warnings (deprecated_coq_lang_lt_08 disabled))
  coqc -q -type-in-type -bt
  -w -deprecated-native-compiler-option
  -w -native-compiler-disabled
  -native-compiler ondemand
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
  -R . foo
  foo.v
