Syntax error inside a cram command
  $ mkdir foo && cd foo
  $ cat >dune-project <<EOF
  > (lang dune 3.0)
  > EOF

  $ cat >t1.t <<EOF
  >   $ foo-bar() { true; }
  > EOF

  $ cd_sed='s/cd.*\&\&.*.sh/cd $DIR \&\& $SUBTEST.sh/'
  $ dune runtest --auto-promote 2>&1 | sed -E -e 's/.+\.sh:/$SUBTEST.sh:/' -e "$cd_sed"
            sh (internal) (exit 2)
  (cd $DIR && $SUBTEST.sh)
  $SUBTEST.sh: line 1: `foo-bar': not a valid identifier
  -> required by alias runtest

  $ cat >t1.t <<EOF
  >   $ exit 1
  >   $ echo foobar
  > EOF
  $ dune runtest --auto-promote 2>&1 | sed -E -e "$cd_sed"
            sh (internal) (exit 1)
  (cd $DIR && $SUBTEST.sh)
  -> required by alias runtest
