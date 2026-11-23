  $ export OUTPUT=output.txt

In normal (non-watching) mode, pwd is the folder from which dune is launched
  $ cd bin
  $ dune exec --root .. -- pwd > $OUTPUT
  $ cat $OUTPUT
  $TESTCASE_ROOT/bin
  $ rm -rf $OUTPUT
  $ cd ..

In watch mode, pwd is also the folder from which dune is launched.
  $ cd bin
  $ dune exec --root .. -w -- pwd > "$OUTPUT" 2> .#dune-stderr &
  $ PID=$!
  $ wait_for_line_with_timeout "$OUTPUT" '..*' 200
  $ kill $PID
  $ wait $PID
  [130]
  $ sed '/, waiting for filesystem changes\.\.\.$/d' .#dune-stderr
  $ cat $OUTPUT
  $TESTCASE_ROOT/bin
  $ rm -rf $OUTPUT
  $ cd ..
