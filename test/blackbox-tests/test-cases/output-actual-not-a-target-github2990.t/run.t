Test for problem in #2990

  $ dune runtest

Make sure that `output.actual` is not inferred as a target:

  $ dune build output.actual
  Error: Don't know how to build output.actual
  [1]
