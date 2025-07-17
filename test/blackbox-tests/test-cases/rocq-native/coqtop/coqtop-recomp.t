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
  > (rocq.theory
  >  (name basic))
  > EOF
  $ cat >dune-project <<EOF
  > (lang dune 3.21)
  > (using rocq 0.11)
  > EOF
  $ dune rocq top --display short --toplevel echo dir/bar.v | ../scrub_coq_args.sh
        coqdep dir/.basic.theory.d
          coqc dir/Nbasic_foo.{cmi,cmxs},dir/foo.{glob,vo}
  -topfile $TESTCASE_ROOT/_build/default/dir/bar.v
  -w -deprecated-native-compiler-option -native-output-dir .
  -native-compiler on
  -nI lib/rocq-runtime/kernel
  -nI $TESTCASE_ROOT/_build/default/dir
  -boot
  -R coq/theories Corelib
  -R $TESTCASE_ROOT/_build/default/dir basic
  $ dune rocq top --display short --toplevel echo dir/bar.v | ../scrub_coq_args.sh
  -topfile $TESTCASE_ROOT/_build/default/dir/bar.v
  -w -deprecated-native-compiler-option -native-output-dir .
  -native-compiler on
  -nI lib/rocq-runtime/kernel
  -nI $TESTCASE_ROOT/_build/default/dir
  -boot
  -R coq/theories Corelib
  -R $TESTCASE_ROOT/_build/default/dir basic
  $ dune clean
  $ (cd dir && dune rocq top --root .. --display short --toplevel echo dir/bar.v) | ../scrub_coq_args.sh
  Entering directory '..'
        coqdep dir/.basic.theory.d
          coqc dir/Nbasic_foo.{cmi,cmxs},dir/foo.{glob,vo}
  Leaving directory '..'
  -topfile $TESTCASE_ROOT/_build/default/dir/bar.v
  -w -deprecated-native-compiler-option -native-output-dir .
  -native-compiler on
  -nI lib/rocq-runtime/kernel
  -nI $TESTCASE_ROOT/_build/default/dir
  -boot
  -R coq/theories Corelib
  -R $TESTCASE_ROOT/_build/default/dir basic
  $ (cd dir && dune rocq top --root .. --display short --toplevel echo dir/bar.v) | ../scrub_coq_args.sh
  Entering directory '..'
  Leaving directory '..'
  -topfile $TESTCASE_ROOT/_build/default/dir/bar.v
  -w -deprecated-native-compiler-option -native-output-dir .
  -native-compiler on
  -nI lib/rocq-runtime/kernel
  -nI $TESTCASE_ROOT/_build/default/dir
  -boot
  -R coq/theories Corelib
  -R $TESTCASE_ROOT/_build/default/dir basic

