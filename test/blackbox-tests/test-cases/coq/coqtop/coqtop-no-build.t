Testing the -no-build option of dune coq top:

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
  > (lang dune 3.8)
  > (using coq 0.8)
  > EOF

On a fresh build, this should do nothing but should pass the correct flags:

  $ dune coq top --no-build --display short --toplevel echo dir/bar.v | ../scrub_coq_args.sh
  -topfile $TESTCASE_ROOT/_build/default/dir/bar.v
  -w -deprecated-native-compiler-option -native-output-dir .
  -native-compiler on
  -nI lib/coq-core/kernel
  -nI $TESTCASE_ROOT/_build/default/dir
  -boot
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

And for comparison normally a build would happen:

  $ dune coq top --display short --toplevel echo dir/bar.v | ../scrub_coq_args.sh
        coqdep dir/.basic.theory.d
          coqc dir/Nbasic_foo.{cmi,cmxs},dir/foo.{glob,vo}
  -topfile $TESTCASE_ROOT/_build/default/dir/bar.v
  -w -deprecated-native-compiler-option -native-output-dir .
  -native-compiler on
  -nI lib/coq-core/kernel
  -nI $TESTCASE_ROOT/_build/default/dir
  -boot
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
