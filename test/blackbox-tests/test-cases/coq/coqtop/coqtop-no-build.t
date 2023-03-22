Testing the -no-build opion of dune coq top:

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
  -w -deprecated-native-compiler-option -native-output-dir . -native-compiler on
  -I coq-core/kernel
  -nI $TESTCASE_ROOT/_build/default/dir
  -R coqtop/_build/default/dir basic

And for comparison normally a build would happen:

  $ dune coq top --display short --toplevel echo dir/bar.v | ../scrub_coq_args.sh
        coqdep dir/.basic.theory.d
          coqc dir/Nbasic_foo.{cmi,cmxs},dir/foo.{glob,vo}
  -topfile $TESTCASE_ROOT/_build/default/dir/bar.v
  -w -deprecated-native-compiler-option -native-output-dir . -native-compiler on
  -I coq-core/kernel
  -nI $TESTCASE_ROOT/_build/default/dir
  -R coqtop/_build/default/dir basic
