Test the "did you mean" hints for dune runtest command.

  $ cat > dune-project <<EOF
  > (lang dune 3.21)
  > EOF

  $ mkdir dir.t other_dir
  $ cat > dir.t/run.t <<EOF
  >   $ echo "Directory-based cram test"
  >   Directory-based cram test
  > EOF
  $ cat > dir_t

  $ dune test dip.t
  Error: "dip.t" does not match any known test.
  Hint: did you mean dir.t?
  [1]

  $ dune test other_dip
  Error: "other_dip" does not match any known test.
  Hint: did you mean other_dir?
  [1]
