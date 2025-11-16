Check that dependencies are not recompiled when this is not necessary. This
was initially an issue, and was reported by @MackieLoeffel in #5457 (see
https://github.com/ocaml/dune/pull/5457#issuecomment-1084161587).

  $ mkdir dir
  $ cat >dir/bar.v <<EOF
  > From basic Require Import foo.
  > Definition mynum (i : mynat) := 3.
  > EOF
  $ cat >dir/foo.v <<EOF
  > Definition mynat := nat.
  > EOF
  $ cat >dir/dune <<EOF
  > (coq.theory
  >  (name basic))
  > EOF
  $ cat >dune-project <<EOF
  > (lang dune 3.0)
  > (using coq 0.3)
  > EOF
  $ dune coq top --display short --toplevel echo dir/bar.v | ../scrub_coq_args.sh
  Warning: Coq Language Versions lower than 0.8 have been deprecated in Dune
  3.8 and will be removed in an upcoming Dune version.
  Hint: To disable this warning, add the following to your dune-project file:
  (warnings (deprecated_coq_lang_lt_08 disabled))
        coqdep dir/.basic.theory.d
          coqc dir/foo.{glob,vo}
  -topfile $TESTCASE_ROOT/_build/default/dir/bar.v
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
  -R $TESTCASE_ROOT/_build/default/dir basic
  $ dune coq top --display short --toplevel echo dir/bar.v | ../scrub_coq_args.sh
  Warning: Coq Language Versions lower than 0.8 have been deprecated in Dune
  3.8 and will be removed in an upcoming Dune version.
  Hint: To disable this warning, add the following to your dune-project file:
  (warnings (deprecated_coq_lang_lt_08 disabled))
  -topfile $TESTCASE_ROOT/_build/default/dir/bar.v
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
  -R $TESTCASE_ROOT/_build/default/dir basic
  $ dune clean
  $ (cd dir && dune coq top --root .. --display short --toplevel echo dir/bar.v) | ../scrub_coq_args.sh
  Entering directory '..'
  Warning: Coq Language Versions lower than 0.8 have been deprecated in Dune
  3.8 and will be removed in an upcoming Dune version.
  Hint: To disable this warning, add the following to your dune-project file:
  (warnings (deprecated_coq_lang_lt_08 disabled))
        coqdep dir/.basic.theory.d
          coqc dir/foo.{glob,vo}
  Leaving directory '..'
  -topfile $TESTCASE_ROOT/_build/default/dir/bar.v
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
  -R $TESTCASE_ROOT/_build/default/dir basic
  $ (cd dir && dune coq top --root .. --display short --toplevel echo dir/bar.v) | ../scrub_coq_args.sh
  Entering directory '..'
  Warning: Coq Language Versions lower than 0.8 have been deprecated in Dune
  3.8 and will be removed in an upcoming Dune version.
  Hint: To disable this warning, add the following to your dune-project file:
  (warnings (deprecated_coq_lang_lt_08 disabled))
  Leaving directory '..'
  -topfile $TESTCASE_ROOT/_build/default/dir/bar.v
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
  -R $TESTCASE_ROOT/_build/default/dir basic

