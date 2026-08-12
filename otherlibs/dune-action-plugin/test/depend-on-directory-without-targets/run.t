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

A generated empty directory is not materialized by the glob dependency, so the
plugin currently fails to read it.

  $ rm -rf some_dir
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
  File "dune", lines 4-6, characters 0-61:
  4 | (rule
  5 |  (alias check-empty)
  6 |  (action (dynamic-run ./foo.exe)))
  read_directory: opendir(some_dir): No such file or directory
  [1]
