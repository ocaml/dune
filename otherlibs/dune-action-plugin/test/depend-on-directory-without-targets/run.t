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

A missing directory has an empty listing when the plugin runs directly.

  $ rm -rf some_dir
  $ ./foo.exe
  Directory listing: []

A generated empty directory also has an empty listing even though the glob
dependency does not materialize it.

  $ cat > dune-project << EOF
  > (lang dune 3.24)
  > (using action-plugin 0.1)
  > EOF
  $ cat > dune << EOF
  > (rule
  >  (targets (dir some_dir))
  >  (action (run mkdir some_dir)))
  > \
  > (rule
  >  (alias check-empty)
  >  (action (dynamic-run ./foo.exe)))
  > EOF
  $ dune build @check-empty
  Directory listing: []
