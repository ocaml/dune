This test checks that 'dynamic-run' can work
when we 'chdir' into different directory.

  $ cp ../bin/foo.exe ./foo_dir/
  $ dune runtest --display short
           foo alias runtest
           foo alias runtest
  Hello from bar!
