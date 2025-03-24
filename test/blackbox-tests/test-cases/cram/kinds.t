Cram supports different kinds of tests.

For any kind of test to work, the following has to be put in dune-project:

  $ cat > dune-project << EOF
  > (lang dune 2.7)
  > (cram enable)
  > EOF

Tests can be in a single file.

  $ mkdir file
  $ cat > file/file.t << EOF
  >   $ echo File test
  > EOF

  $ dune runtest file
  File "file/file.t", line 1, characters 0-0:
  Error: Files _build/default/file/file.t and
  _build/default/file/file.t.corrected differ.
  [1]

  $ dune promote file/file.t
  Promoting _build/default/file/file.t.corrected to file/file.t.

  $ cat file/file.t
    $ echo File test
    File test

  $ dune runtest file

They can be in a test directory. In this case, a run.t file must be present. All
other files are visible within the test.

  $ mkdir -p dir/dir.t
  $ echo "Contents of file a" > dir/dir.t/a
  $ cat > dir/dir.t/run.t << EOF
  >   $ echo Dir test
  >  
  >   $ cat a
  > EOF

  $ dune runtest dir
  File "dir/dir.t/run.t", line 1, characters 0-0:
  Error: Files _build/default/dir/dir.t/run.t and
  _build/default/dir/dir.t/run.t.corrected differ.
  [1]

  $ dune promote dir/dir.t/run.t
  Promoting _build/default/dir/dir.t/run.t.corrected to dir/dir.t/run.t.

  $ cat dir/dir.t/run.t
    $ echo Dir test
    Dir test
   
    $ cat a
    Contents of file a

  $ dune runtest dir

If there is no run.t file, an error message is displayed.

  $ mkdir -p dir-no-run/dir.t
  $ echo "Contents of file a" > dir-no-run/dir.t/a
  $ dune runtest dir-no-run
  File "dir-no-run/dir.t", line 1, characters 0-0:
  Error: Cram test directory dir-no-run/dir.t does not contain a run.t file.
  [1]

However, if the directory is empty, this check is skipped. (git can leave such
empty directories)

  $ mkdir -p dir-empty/dir.t
  $ dune runtest dir-empty
