  $ dune runtest --force --display short
  File "dune", line 37, characters 0-145:
  37 | (library
  38 |  (name foobar_ppd)
  39 |  (public_name foobar.ppd)
  40 |  (synopsis "pp'd with a rewriter")
  41 |  (libraries foobar)
  42 |  (preprocess (pps foobar_rewriter)))
  Error: mode native isn't available. It's not provided in the dependency:
  foobar.baz
  [1]
