Regression test for installing mli only modules. Previously, the install step
would fail because the .cmi wasn't correctly copied to the _build/install dir.

  $ dune build @install
  $ dune runtest
  testing
  $ dune install --prefix ./installed --display short 2>&1 | grep -i cmi
  Installing installed/lib/foobar/foobar.cmi
  Installing installed/lib/foobar/impl/foobar.cmi
