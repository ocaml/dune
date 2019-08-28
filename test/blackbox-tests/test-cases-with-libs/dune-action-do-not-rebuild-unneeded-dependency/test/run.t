This test checks that in case the dependency of multi staged computation changes,
only the dependencies up to this stage are rebuild.

  $ cp ../bin/client.exe ./
  $ dune runtest --display short
        client alias runtest
  Building foo_or_bar!
        client alias runtest
  Building foo!
        client alias runtest
  Hello from foo!
  $ printf "bar" > foo_or_bar_source
  $ printf "Hello from foo again!" > foo_source
  $ printf "Hello from bar for the first time!" > bar_source
  $ dune runtest --display short
  Building foo_or_bar!
        client alias runtest
        client alias runtest
  Building bar!
        client alias runtest
  Hello from bar for the first time!
