This test check that we can preprocess source code with actions

  $ dune runtest --display short 2>&1 | grep "  pp"
            pp dune/test.pp.ml
