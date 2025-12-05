Cram tests can forbid conflicts:

  $ cat >dune-project <<EOF
  > (lang dune 3.21)
  > EOF

  $ cat >dune <<EOF
  > (cram (conflict_markers error))
  > EOF

Full conflict without command and output interleaving:

  $ cat >test.t <<EOF
  > <<<<<<<
  > A Small
  > =======
  > Conflict
  > >>>>>>>
  > $ echo tada
  > EOF

  $ dune runtest test.t
  File "test.t", lines 1-6, characters 0-52:
  1 | <<<<<<<
  2 | A Small
  3 | =======
  4 | Conflict
  5 | >>>>>>>
  6 | $ echo tada
  Error: Conflict marker found. Please remove it or set (conflict_markers
  allow)
  [1]

Full conflict with command and output interleaving:

  $ cat >test.t <<EOF
  > <<<<<<<
  >   $ foo
  > =======
  >   > bar
  > >>>>>>>
  > $ echo tada
  > EOF

  $ dune runtest test.t
  File "test.t", lines 1-6, characters 0-51:
  1 | <<<<<<<
  2 |   $ foo
  3 | =======
  4 |   > bar
  5 | >>>>>>>
  6 | $ echo tada
  Error: Conflict marker found. Please remove it or set (conflict_markers
  allow)
  [1]

Partial conflicts are ignored:

  $ cat >test.t <<EOF
  > <<<<<<<
  >   $ foo
  >   > bar
  > >>>>>>>
  > EOF

  $ dune runtest test.t
  File "test.t", line 1, characters 0-0:
  Error: Files _build/default/test.t and _build/default/test.t.corrected
  differ.
  [1]

  $ cat >test.t <<EOF
  > <<<<<<<
  >   $ foo
  > =======
  >   > bar
  > EOF

  $ dune runtest test.t
  File "test.t", line 1, characters 0-0:
  Error: Files _build/default/test.t and _build/default/test.t.corrected
  differ.
  [1]
