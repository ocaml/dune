  $ echo 'Hello, world!' > hello.expected

  $ dune runtest

  $ cp hello.wrong-output hello.expected
  $ dune runtest
  File "hello.expected", line 1, characters 0-0:
  Error: Files _build/default/hello.expected and _build/default/hello.output
  differ.
  [1]
  $ dune promote
  Promoting _build/default/hello.output to hello.expected.
  $ cat hello.expected
  Hello, world!

  $ dune build @cmp
  File "dune", lines 12-14, characters 0-41:
  12 | (alias
  13 |  (name   cmp)
  14 |  (action (cmp a b)))
  Error: Files _build/default/a and _build/default/b differ.
  [1]
