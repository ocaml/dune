Testing the -no-build option of dune rocq top:

  $ mkdir dir
  $ cat >dir/bar.v <<EOF
  > From basic Require Import foo.
  > Definition mynum (i : mynat) := 3.
  > EOF
  $ cat >dir/foo.v <<EOF
  > Definition mynat := nat.
  > EOF
  $ cat >dir/dune <<EOF
  > (rocq.theory
  >  (name basic))
  > EOF
  $ cat >dune-project <<EOF
  > (lang dune 3.21)
  > (using rocq 0.11)
  > EOF

On a fresh build, this should do nothing but should pass the correct flags:

  $ dune rocq top --no-build --display short --toplevel echo dir/bar.v | ../scrub_coq_args.sh
  -topfile $TESTCASE_ROOT/_build/default/dir/bar.v
  -w -deprecated-native-compiler-option
  -w -native-compiler-disabled
  -native-compiler ondemand
  -boot
  -R coq/theories Corelib
  -R $TESTCASE_ROOT/_build/default/dir basic

And for comparison normally a build would happen:

  $ dune rocq top --display short --toplevel echo dir/bar.v | ../scrub_coq_args.sh
          rocq dir/.basic.theory.d
          rocq dir/foo.{glob,vo}
  -topfile $TESTCASE_ROOT/_build/default/dir/bar.v
  -w -deprecated-native-compiler-option
  -w -native-compiler-disabled
  -native-compiler ondemand
  -boot
  -R coq/theories Corelib
  -R $TESTCASE_ROOT/_build/default/dir basic
