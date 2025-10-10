Demonstrate what happens if a user attempts to add to modify the PATH variable
using the withenv action.

  $ . ./helpers.sh

This path is system-specific so we need to be able to remove it from the output.
  $ DUNE_PATH=$(dirname $(which dune))

  $ make_lockdir

Make some packages so that the test package can have dependencies:
  $ make_lockpkg hello1 <<EOF
  > (version 0.0.1)
  > EOF
  $ make_lockpkg hello2.pkg <<EOF
  > (version 0.0.1)
  > EOF

Printing out PATH without setting it:
  $ make_lockpkg test <<'EOF'
  > (version 0.0.1)
  > (build
  >  (system "echo PATH=$PATH"))
  > EOF
  $ dune clean
  $ PATH=$DUNE_PATH:/bin build_pkg test 2>&1 | sed -e "s#$DUNE_PATH#DUNE_PATH#"
  PATH=DUNE_PATH:/bin

Setting PATH to a specific value:
  $ make_lockpkg test <<'EOF'
  > (version 0.0.1)
  > (build
  >  (withenv
  >   ((= PATH /tmp/bin))
  >   (system "echo PATH=$PATH")))
  > EOF
  $ dune clean
  $ PATH=$DUNE_PATH:/bin build_pkg test 2>&1 | sed -e "s#$DUNE_PATH#DUNE_PATH#"
  PATH=/tmp/bin

Attempting to add a path to PATH replaces the entire PATH:
  $ make_lockpkg test <<'EOF'
  > (version 0.0.1)
  > (build
  >  (withenv
  >   ((+= PATH /tmp/bin))
  >   (system "echo PATH=$PATH")))
  > EOF
  $ dune clean
  $ PATH=$DUNE_PATH:/bin build_pkg test 2>&1 | sed -e "s#$DUNE_PATH#DUNE_PATH#"
  PATH=/tmp/bin:DUNE_PATH:/bin

Try adding multiple paths to PATH:
  $ make_lockpkg test <<'EOF'
  > (version 0.0.1)
  > (build
  >  (withenv
  >   ((+= PATH /tmp/bin)
  >    (+= PATH /foo/bin)
  >    (+= PATH /bar/bin))
  >   (system "echo PATH=$PATH")))
  > EOF
  $ dune clean
  $ PATH=$DUNE_PATH:/bin build_pkg test 2>&1 | sed -e "s#$DUNE_PATH#DUNE_PATH#"
  PATH=/bar/bin:/foo/bin:/tmp/bin:DUNE_PATH:/bin

Printing out PATH without setting it when the package has a dependency:
  $ make_lockpkg test <<'EOF'
  > (version 0.0.1)
  > (depends hello1 hello2)
  > (build
  >  (system "echo PATH=$PATH"))
  > EOF
  $ dune clean
  $ OCAMLRUNPARAM=b PATH=$DUNE_PATH:/bin build_pkg test 2>&1 | sed -e "s#$DUNE_PATH#DUNE_PATH#"
  PATH=$TESTCASE_ROOT/_build/_private/default/.pkg/hello2.0.0.1-3cf268d89ba7f04a10a17a1a00d6d508/target/bin:$TESTCASE_ROOT/_build/_private/default/.pkg/hello1.0.0.1-2bbe9250d988b3a1dc98ca2cf6f9ab0c/target/bin:DUNE_PATH:/bin

Setting PATH to a specific value:
  $ make_lockpkg test <<'EOF'
  > (version 0.0.1)
  > (depends hello1 hello2)
  > (build
  >  (withenv
  >   ((= PATH /tmp/bin))
  >   (system "echo PATH=$PATH")))
  > EOF
  $ dune clean
  $ PATH=$DUNE_PATH:/bin build_pkg test 2>&1 | sed -e "s#$DUNE_PATH#DUNE_PATH#"
  PATH=/tmp/bin

Attempting to add a path to PATH replaces the entire PATH:
  $ make_lockpkg test <<'EOF'
  > (version 0.0.1)
  > (depends hello1 hello2)
  > (build
  >  (withenv
  >   ((+= PATH /tmp/bin))
  >   (system "echo PATH=$PATH")))
  > EOF
  $ dune clean
  $ PATH=$DUNE_PATH:/bin build_pkg test 2>&1 | sed -e "s#$DUNE_PATH#DUNE_PATH#"
  PATH=/tmp/bin:$TESTCASE_ROOT/_build/_private/default/.pkg/hello2.0.0.1-3cf268d89ba7f04a10a17a1a00d6d508/target/bin:$TESTCASE_ROOT/_build/_private/default/.pkg/hello1.0.0.1-2bbe9250d988b3a1dc98ca2cf6f9ab0c/target/bin:DUNE_PATH:/bin

Try adding multiple paths to PATH:
  $ make_lockpkg test <<'EOF'
  > (version 0.0.1)
  > (depends hello1 hello2)
  > (build
  >  (withenv
  >   ((+= PATH /tmp/bin)
  >    (+= PATH /foo/bin)
  >    (+= PATH /bar/bin))
  >   (system "echo PATH=$PATH")))
  > EOF
  $ dune clean
  $ PATH=$DUNE_PATH:/bin build_pkg test 2>&1 | sed -e "s#$DUNE_PATH#DUNE_PATH#"
  PATH=/bar/bin:/foo/bin:/tmp/bin:$TESTCASE_ROOT/_build/_private/default/.pkg/hello2.0.0.1-3cf268d89ba7f04a10a17a1a00d6d508/target/bin:$TESTCASE_ROOT/_build/_private/default/.pkg/hello1.0.0.1-2bbe9250d988b3a1dc98ca2cf6f9ab0c/target/bin:DUNE_PATH:/bin
