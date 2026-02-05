Test that the %{jobs} variable matches the configured concurrency.

  $ cat >dune-project <<EOF
  > (lang dune 3.22)
  > EOF
  $ cat >dune <<EOF
  > (rule (alias runtest) (action (echo "make -j%{jobs}")))
  > EOF
  $ dune runtest -j3615
  File "dune", line 1, characters 44-51:
  1 | (rule (alias runtest) (action (echo "make -j%{jobs}")))
                                                  ^^^^^^^
  Error: Unknown variable %{jobs}
  [1]
  $ cat >dune <<EOF
  > (rule (alias runtest) (action (echo "make -j%{jobs}")))
  > EOF
  $ dune runtest -jauto
  File "dune", line 1, characters 44-51:
  1 | (rule (alias runtest) (action (echo "make -j%{jobs}")))
                                                  ^^^^^^^
  Error: Unknown variable %{jobs}
  [1]
