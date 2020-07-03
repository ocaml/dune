  $ dune runtest --root singular
  Entering directory 'singular'
      singular alias runtest
  singular test

  $ dune runtest --root plural
  Entering directory 'plural'
  regular_test alias runtest
  regular test
  regular_test2 alias runtest
  regular test2
  $ dune runtest --root generated
  Entering directory 'generated'
  File "generated.expected", line 1, characters 0-0:
  Error: Files _build/default/generated.expected and
  _build/default/generated.output differ.
  [1]
