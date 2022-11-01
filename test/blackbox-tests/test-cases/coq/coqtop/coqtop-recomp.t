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
  $ dune coq top --display short --toplevel echo dir/bar.v
        coqdep dir/bar.v.d
        coqdep dir/foo.v.d
          coqc dir/foo.{glob,vo}
  -topfile $TESTCASE_ROOT/_build/default/dir/bar.v -q -w -deprecated-native-compiler-option -w -native-compiler-disabled -native-compiler ondemand -R $TESTCASE_ROOT/_build/default/dir basic
  $ dune coq top --display short --toplevel echo dir/bar.v
  -topfile $TESTCASE_ROOT/_build/default/dir/bar.v -q -w -deprecated-native-compiler-option -w -native-compiler-disabled -native-compiler ondemand -R $TESTCASE_ROOT/_build/default/dir basic
  $ dune clean
  $ (cd dir && dune coq top --root .. --display short --toplevel echo dir/bar.v)
  Entering directory '..'
        coqdep dir/bar.v.d
        coqdep dir/foo.v.d
          coqc dir/foo.{glob,vo}
  -topfile $TESTCASE_ROOT/_build/default/dir/bar.v -q -w -deprecated-native-compiler-option -w -native-compiler-disabled -native-compiler ondemand -R $TESTCASE_ROOT/_build/default/dir basic
  $ (cd dir && dune coq top --root .. --display short --toplevel echo dir/bar.v)
  Entering directory '..'
  -topfile $TESTCASE_ROOT/_build/default/dir/bar.v -q -w -deprecated-native-compiler-option -w -native-compiler-disabled -native-compiler ondemand -R $TESTCASE_ROOT/_build/default/dir basic

