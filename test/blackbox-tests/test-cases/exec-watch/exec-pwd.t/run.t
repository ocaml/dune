  $ export OUTPUT=output.txt

In normal (non-watching) mode, pwd is the folder from which dune is launched
  $ cd bin
  $ dune exec --root .. -- pwd > $OUTPUT
  Entering directory '..'
  Leaving directory '..'
  $ cat $OUTPUT
  $TESTCASE_ROOT/bin
  $ rm -rf $OUTPUT
  $ cd ..

In watch mode, pwd is also the folder from which dune is launched.
  $ cd bin
  $ dune exec --root .. -w -- pwd > $OUTPUT &
  Entering directory '..'
  Success, waiting for filesystem changes...
  Leaving directory '..'
  $ PID=$!
  $ until test -s $OUTPUT; do sleep 0.1; done;
  $ kill $PID
  $ cat $OUTPUT
  $TESTCASE_ROOT/bin
  $ rm -rf $OUTPUT
  $ cd ..
