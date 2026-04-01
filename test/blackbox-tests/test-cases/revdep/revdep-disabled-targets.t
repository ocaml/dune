Do not build disabled executables or tests when following revdeps.
The (enabled_if false) on executable/tests stanzas prevents those directories
from being added as reverse dependencies, so their @default artifacts (e.g.
the (rule (alias default) ...) stanzas) are never triggered.

  $ make_dune_project 3.20

  $ mkdir core tool_on tool_off tests_off

  $ cat >core/dune <<'EOF'
  > (library (name corelib))
  > EOF

  $ cat >tool_on/dune <<'EOF'
  > (executable (name on) (libraries corelib))
  > (rule (alias default) (targets on.built) (action (with-stdout-to %{targets} (echo ON))))
  > EOF
  $ cat >tool_on/on.ml <<'EOF'
  > let () = ()
  > EOF

  $ cat >tool_off/dune <<'EOF'
  > (executable (name off) (libraries corelib) (enabled_if false))
  > (rule (alias default) (targets off.built) (action (with-stdout-to %{targets} (echo OFF))))
  > EOF
  $ cat >tool_off/off.ml <<'EOF'
  > let () = ()
  > EOF

  $ cat >tests_off/dune <<'EOF'
  > (tests
  >  (names t1)
  >  (libraries corelib)
  >  (enabled_if false))
  > (rule (alias default) (targets test_off.built) (action (with-stdout-to %{targets} (echo TESTOFF))))
  > EOF
  $ cat >tests_off/t1.ml <<'EOF'
  > let () = ()
  > EOF

  $ dune build @core/revdep
  $ cat _build/default/tool_on/on.built
  ON
  $ test -e _build/default/tool_off/off.built && echo "should not build off" || echo "off not built"
  off not built
  $ test -e _build/default/tests_off/test_off.built && echo "should not build tests" || echo "tests not built"
  should not build tests
