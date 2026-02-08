Test that the %{jobs} variable matches the configured concurrency.

  $ cat >dune-project <<EOF
  > (lang dune 3.22)
  > EOF
  $ cat >dune <<EOF
  > (rule (alias runtest) (action (echo "make -j%{jobs}")))
  > EOF
  $ dune runtest -j3615
  make -j3615
  $ cat >dune <<EOF
  > (rule (alias runtest) (action (echo "make -j%{jobs}")))
  > EOF
  $ dune runtest -jauto
  make -j16
