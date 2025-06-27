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
          coqc dir/foo.{glob,vo}
  -topfile $TESTCASE_ROOT/_build/default/dir/bar.v
  -w -deprecated-native-compiler-option
  -w -native-compiler-disabled
  -native-compiler ondemand
  -boot
  -I lib/coq/../rocq-runtime/plugins/btauto
  -I lib/coq/../rocq-runtime/plugins/cc
  -I lib/coq/../rocq-runtime/plugins/cc_core
  -I lib/coq/../rocq-runtime/plugins/derive
  -I lib/coq/../rocq-runtime/plugins/extraction
  -I lib/coq/../rocq-runtime/plugins/firstorder
  -I lib/coq/../rocq-runtime/plugins/firstorder_core
  -I lib/coq/../rocq-runtime/plugins/funind
  -I lib/coq/../rocq-runtime/plugins/ltac
  -I lib/coq/../rocq-runtime/plugins/ltac2
  -I lib/coq/../rocq-runtime/plugins/ltac2_ltac1
  -I lib/coq/../rocq-runtime/plugins/micromega
  -I lib/coq/../rocq-runtime/plugins/micromega_core
  -I lib/coq/../rocq-runtime/plugins/nsatz
  -I lib/coq/../rocq-runtime/plugins/nsatz_core
  -I lib/coq/../rocq-runtime/plugins/number_string_notation
  -I lib/coq/../rocq-runtime/plugins/ring
  -I lib/coq/../rocq-runtime/plugins/rtauto
  -I lib/coq/../rocq-runtime/plugins/ssreflect
  -I lib/coq/../rocq-runtime/plugins/ssrmatching
  -I lib/coq/../rocq-runtime/plugins/tauto
  -I lib/coq/../rocq-runtime/plugins/tutorial/p0
  -I lib/coq/../rocq-runtime/plugins/tutorial/p1
  -I lib/coq/../rocq-runtime/plugins/tutorial/p2
  -I lib/coq/../rocq-runtime/plugins/tutorial/p3
  -I lib/coq/../rocq-runtime/plugins/tutorial/p4
  -I lib/coq/../rocq-runtime/plugins/zify
  -R coq/theories Corelib
  -R $TESTCASE_ROOT/_build/default/dir basic
  $ dune rocq top --display short --toplevel echo dir/bar.v | ../scrub_coq_args.sh
  -topfile $TESTCASE_ROOT/_build/default/dir/bar.v
  -w -deprecated-native-compiler-option
  -w -native-compiler-disabled
  -native-compiler ondemand
  -boot
  -I lib/coq/../rocq-runtime/plugins/btauto
  -I lib/coq/../rocq-runtime/plugins/cc
  -I lib/coq/../rocq-runtime/plugins/cc_core
  -I lib/coq/../rocq-runtime/plugins/derive
  -I lib/coq/../rocq-runtime/plugins/extraction
  -I lib/coq/../rocq-runtime/plugins/firstorder
  -I lib/coq/../rocq-runtime/plugins/firstorder_core
  -I lib/coq/../rocq-runtime/plugins/funind
  -I lib/coq/../rocq-runtime/plugins/ltac
  -I lib/coq/../rocq-runtime/plugins/ltac2
  -I lib/coq/../rocq-runtime/plugins/ltac2_ltac1
  -I lib/coq/../rocq-runtime/plugins/micromega
  -I lib/coq/../rocq-runtime/plugins/micromega_core
  -I lib/coq/../rocq-runtime/plugins/nsatz
  -I lib/coq/../rocq-runtime/plugins/nsatz_core
  -I lib/coq/../rocq-runtime/plugins/number_string_notation
  -I lib/coq/../rocq-runtime/plugins/ring
  -I lib/coq/../rocq-runtime/plugins/rtauto
  -I lib/coq/../rocq-runtime/plugins/ssreflect
  -I lib/coq/../rocq-runtime/plugins/ssrmatching
  -I lib/coq/../rocq-runtime/plugins/tauto
  -I lib/coq/../rocq-runtime/plugins/tutorial/p0
  -I lib/coq/../rocq-runtime/plugins/tutorial/p1
  -I lib/coq/../rocq-runtime/plugins/tutorial/p2
  -I lib/coq/../rocq-runtime/plugins/tutorial/p3
  -I lib/coq/../rocq-runtime/plugins/tutorial/p4
  -I lib/coq/../rocq-runtime/plugins/zify
  -R coq/theories Corelib
  -R $TESTCASE_ROOT/_build/default/dir basic
  $ dune clean
  $ (cd dir && dune rocq top --root .. --display short --toplevel echo dir/bar.v) | ../scrub_coq_args.sh
  Entering directory '..'
        coqdep dir/.basic.theory.d
          coqc dir/foo.{glob,vo}
  Leaving directory '..'
  -topfile $TESTCASE_ROOT/_build/default/dir/bar.v
  -w -deprecated-native-compiler-option
  -w -native-compiler-disabled
  -native-compiler ondemand
  -boot
  -I lib/coq/../rocq-runtime/plugins/btauto
  -I lib/coq/../rocq-runtime/plugins/cc
  -I lib/coq/../rocq-runtime/plugins/cc_core
  -I lib/coq/../rocq-runtime/plugins/derive
  -I lib/coq/../rocq-runtime/plugins/extraction
  -I lib/coq/../rocq-runtime/plugins/firstorder
  -I lib/coq/../rocq-runtime/plugins/firstorder_core
  -I lib/coq/../rocq-runtime/plugins/funind
  -I lib/coq/../rocq-runtime/plugins/ltac
  -I lib/coq/../rocq-runtime/plugins/ltac2
  -I lib/coq/../rocq-runtime/plugins/ltac2_ltac1
  -I lib/coq/../rocq-runtime/plugins/micromega
  -I lib/coq/../rocq-runtime/plugins/micromega_core
  -I lib/coq/../rocq-runtime/plugins/nsatz
  -I lib/coq/../rocq-runtime/plugins/nsatz_core
  -I lib/coq/../rocq-runtime/plugins/number_string_notation
  -I lib/coq/../rocq-runtime/plugins/ring
  -I lib/coq/../rocq-runtime/plugins/rtauto
  -I lib/coq/../rocq-runtime/plugins/ssreflect
  -I lib/coq/../rocq-runtime/plugins/ssrmatching
  -I lib/coq/../rocq-runtime/plugins/tauto
  -I lib/coq/../rocq-runtime/plugins/tutorial/p0
  -I lib/coq/../rocq-runtime/plugins/tutorial/p1
  -I lib/coq/../rocq-runtime/plugins/tutorial/p2
  -I lib/coq/../rocq-runtime/plugins/tutorial/p3
  -I lib/coq/../rocq-runtime/plugins/tutorial/p4
  -I lib/coq/../rocq-runtime/plugins/zify
  -R coq/theories Corelib
  -R $TESTCASE_ROOT/_build/default/dir basic
  $ (cd dir && dune rocq top --root .. --display short --toplevel echo dir/bar.v) | ../scrub_coq_args.sh
  Entering directory '..'
  Leaving directory '..'
  -topfile $TESTCASE_ROOT/_build/default/dir/bar.v
  -w -deprecated-native-compiler-option
  -w -native-compiler-disabled
  -native-compiler ondemand
  -boot
  -I lib/coq/../rocq-runtime/plugins/btauto
  -I lib/coq/../rocq-runtime/plugins/cc
  -I lib/coq/../rocq-runtime/plugins/cc_core
  -I lib/coq/../rocq-runtime/plugins/derive
  -I lib/coq/../rocq-runtime/plugins/extraction
  -I lib/coq/../rocq-runtime/plugins/firstorder
  -I lib/coq/../rocq-runtime/plugins/firstorder_core
  -I lib/coq/../rocq-runtime/plugins/funind
  -I lib/coq/../rocq-runtime/plugins/ltac
  -I lib/coq/../rocq-runtime/plugins/ltac2
  -I lib/coq/../rocq-runtime/plugins/ltac2_ltac1
  -I lib/coq/../rocq-runtime/plugins/micromega
  -I lib/coq/../rocq-runtime/plugins/micromega_core
  -I lib/coq/../rocq-runtime/plugins/nsatz
  -I lib/coq/../rocq-runtime/plugins/nsatz_core
  -I lib/coq/../rocq-runtime/plugins/number_string_notation
  -I lib/coq/../rocq-runtime/plugins/ring
  -I lib/coq/../rocq-runtime/plugins/rtauto
  -I lib/coq/../rocq-runtime/plugins/ssreflect
  -I lib/coq/../rocq-runtime/plugins/ssrmatching
  -I lib/coq/../rocq-runtime/plugins/tauto
  -I lib/coq/../rocq-runtime/plugins/tutorial/p0
  -I lib/coq/../rocq-runtime/plugins/tutorial/p1
  -I lib/coq/../rocq-runtime/plugins/tutorial/p2
  -I lib/coq/../rocq-runtime/plugins/tutorial/p3
  -I lib/coq/../rocq-runtime/plugins/tutorial/p4
  -I lib/coq/../rocq-runtime/plugins/zify
  -R coq/theories Corelib
  -R $TESTCASE_ROOT/_build/default/dir basic

