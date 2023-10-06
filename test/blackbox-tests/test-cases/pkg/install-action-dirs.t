Install actions should have the switch directory prepared:

  $ . ./helpers.sh

  $ make_lockdir
  $ cat >dune.lock/test.pkg <<'EOF'
  > (install (system "find %{prefix} | sort"))
  > EOF

  $ build_pkg test
  $TESTCASE_ROOT/_build/_private/default/.pkg/test/target
  $TESTCASE_ROOT/_build/_private/default/.pkg/test/target/bin
  $TESTCASE_ROOT/_build/_private/default/.pkg/test/target/doc
  $TESTCASE_ROOT/_build/_private/default/.pkg/test/target/doc/test
  $TESTCASE_ROOT/_build/_private/default/.pkg/test/target/etc
  $TESTCASE_ROOT/_build/_private/default/.pkg/test/target/etc/test
  $TESTCASE_ROOT/_build/_private/default/.pkg/test/target/lib
  $TESTCASE_ROOT/_build/_private/default/.pkg/test/target/lib/stublibs
  $TESTCASE_ROOT/_build/_private/default/.pkg/test/target/lib/test
  $TESTCASE_ROOT/_build/_private/default/.pkg/test/target/lib/toplevel
  $TESTCASE_ROOT/_build/_private/default/.pkg/test/target/man
  $TESTCASE_ROOT/_build/_private/default/.pkg/test/target/sbin
  $TESTCASE_ROOT/_build/_private/default/.pkg/test/target/share
  $TESTCASE_ROOT/_build/_private/default/.pkg/test/target/share/test
