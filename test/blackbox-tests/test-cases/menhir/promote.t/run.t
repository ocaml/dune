Reproduction case for #1781, only the .ml and .mli should be promoted:

  $ dune build @all
  $ ls -1 _build/default | sort | grep mock
  parser__mock.ml.mock
  parser__mock.mli.inferred
  $ ls -1 | grep mock
  [1]
Check what is being generated exactly:
  $ ls -1
  _build
  dune
  dune-project
  parser.ml
  parser.mli
  parser.mly
