  $ cat > dune-project << EOF
  > (lang dune 2.0)
  > (using action-plugin 0.1)
  > EOF

  $ cat > dune << EOF
  > (data_only_dirs some_dir)
  > \
  > (rule
  >  (alias runtest)
  >  (action (dynamic-run ./foo.exe)))
  > EOF

  $ mkdir some_dir
  $ touch some_dir/some_file1
  $ touch some_dir/some_file2

  $ cp ./bin/foo.exe ./

  $ dune runtest
  Directory listing: [some_file1; some_file2]
