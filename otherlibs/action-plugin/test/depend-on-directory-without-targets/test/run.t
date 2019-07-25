  $ echo "(lang dune 2.0)" > dune-project

  $ cat > dune << EOF
  > (data_only_dirs some_dir)
  > \
  > (alias
  >  (name runtest)
  >  (action (dynamic-run ./foo.exe)))
  > EOF

  $ mkdir some_dir
  $ touch some_dir/some_file1
  $ touch some_dir/some_file2

  $ cp ../bin/foo.exe ./

  $ dune runtest --display short
           foo alias runtest
           foo alias runtest
  Directory listing: [some_file1; some_file2]
