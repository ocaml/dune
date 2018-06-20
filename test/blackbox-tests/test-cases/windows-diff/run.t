  $ dune runtest

  $ cp hello.wrong-output hello.expected
  $ dune runtest --diff-command false 2>&1 | sed 's/.*false.*/DIFF/;s/.*internal.*/DIFF/'
  DIFF
  DIFF
  $ dune promote
  Promoting _build/default/hello.output to hello.expected.
  $ cat hello.expected
  Hello, world!

  $ dune build @cmp
  Error: Files _build/default/a and _build/default/b differ.
  [1]
